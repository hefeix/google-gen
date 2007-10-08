// Copyright (C) 2006 Google Inc. and Georges Harik
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// 
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: Noam Shazeer and Georges Harik

#include "spec.h"
#include "model.h"

void Given::L1_Init(OTuple tuple) {
  Base::L1_Init();
  posting_ = new OwnedPosting(tuple, CREATION, this);
}

void Given::L1_Erase() {
  posting_->L1_Erase();
  Base::L1_Erase();
}

void Requirement::L1_Init(OTuple tuple) {
  Base::L1_Init();
  CL.InsertIntoSet(&M.requirements_, this);
  tuple_ = tuple;
  Query * q = BB.L1_GetExecuteQuery(OPattern::Make(Pattern(1, tuple)), 
				    SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT, this);
  if (q->GetCount() == 0) L1_AddViolation();
}
void Requirement::L1_Erase() {
  subscription_->L1_Erase();
  L1_EraseOwnedViolations(this);
  CL.RemoveFromSet(&M.requirements_, this);  
  Base::L1_Erase();
}
void Requirement::Update(const QueryUpdate &update, SubType * sub){
  int count = subscription_->subscribee_->GetCount();
  Violation * violation = FindViolation(this, Violation::REQUIREMENT);
  if (violation && count != 0) {
    L1_RemoveViolation();
  }
  if (!violation && count == 0) {
    L1_AddViolation();
  }
}
void Requirement::L1_AddViolation() {
  New<RequirementViolation>(this);
}
void Requirement::L1_RemoveViolation() {
  Violation * violation = FindViolation(this, Violation::REQUIREMENT);
  violation->L1_Erase();
}

void Prohibition::L1_Init(OTuple tuple) {
  Base::L1_Init();
  CL.InsertIntoSet(&M.prohibitions_, this);
  tuple_ = tuple;
  Query * q = BB.L1_GetExecuteQuery(OPattern::Make(Pattern(1, tuple)), 
				    SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT | UPDATE_WHICH, this);
  subscription_->L1_SendCurrentAsUpdates();
}
void Prohibition::L1_Erase() {
  subscription_->L1_Erase();
  while (violations_.size()) {
    violations_.begin()->second->L1_Erase();    
  }
  CL.RemoveFromSet(&M.prohibitions_, this);
  Base::L1_Erase();
}
void Prohibition::Update(const QueryUpdate &update, SubType * sub){
  forall(run, update.changes_) {
    if (run->action_ == UPDATE_CREATE) {
      OTuple t = Substitute(run->data_.Data(), tuple_);
      if (exceptions_ % t) continue;
      CHECK(!(violations_ % t));
      New<ProhibitionViolation>(this, t);
    }
    if (run->action_ == UPDATE_DESTROY) {
      OTuple t = Substitute(run->data_.Data(), tuple_);
      if (exceptions_ % t) {
	CHECK(!(violations_ % t));
	continue;
      }
      Violation ** v = violations_ % t;
      CHECK(v);
      (*v)->L1_Erase();
    }
    if (run->action_ == UPDATE_CHANGE_TIME) {
      OTuple t = Substitute(run->data_.Data(), tuple_);
      Violation ** v = violations_ % t;
      CHECK(v);
      (*v)->N1_ComputedTimeChanged();
    }
  }
}
void Prohibition::AddException(OTuple t) {
  CHECK(!(exceptions_ % t));
  CL.InsertIntoSet(&exceptions_, t);
  Violation ** v = violations_ % t;
  if (v) (*v)->L1_Erase();
}

Record Requirement::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  ret["tuple"] = tuple_.ToString();
  return ret;
}
Record Prohibition::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  ret["tuple"] = tuple_.ToString();
  forall(run, exceptions_) ret["exceptions"] += run->ToString();
  return ret;
}
Record Given::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  ret["posting"] = posting_->ShortDescription();
  return ret;
}



void LoadSpec(istream & input) {
  Object o;
  OTuple t;
  Prohibition * last_prohibition = NULL;
  while (input >> o) {
    if (o.GetType() == Object::KEYWORD) {
      Keyword k = o;
      CHECK(input >> t);
      if (k.Data() == "prohibition") {
	last_prohibition = Prohibition::Make(t);
	continue;
      }
      if (k.Data() == "given") {
	Given::Make(t);
	last_prohibition = NULL;
	continue;
      }
      if (k.Data() == "exception") {
	CHECK(last_prohibition);
	last_prohibition->AddException(t);
	continue;
      }
      CHECK(false);
    }
    // it's a requirement/functional requirement
    t = o;
    last_prohibition = NULL;
    Tuple required_tuple;
    Tuple prohibited_tuple;
    bool any_wildcards = false;
    for (uint i=0; i<t.Data().size(); i++) {
      if (t.Data()[i] == WILDCARD) {
	any_wildcards = true;
	prohibited_tuple.push_back(WILDCARD);
	i++;
	CHECK(i<t.Data().size());
      } else {
	prohibited_tuple.push_back(t.Data()[i]);
      }
      required_tuple.push_back(t.Data()[i]);
    }
    Requirement::Make(OTuple::Make(required_tuple));
    if(any_wildcards) {
      Prohibition * p = Prohibition::Make(OTuple::Make(prohibited_tuple));
      p->AddException(OTuple::Make(required_tuple));
    }
  }
}
