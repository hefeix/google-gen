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

Requirement::Requirement(OTuple tuple) {
  CL.InsertIntoSet(&M.requirements_, this);
  tuple_ = tuple;
  Query * q = BB.L1_GetExecuteQuery(OPattern::Make(Pattern(1, tuple)), 
				    SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT, this);
  if (q->GetCount() == 0) L1_AddViolation();
}
void Requirement::L1_Erase() {
  subscription_->L1_Erase();
  EraseOwnedViolations(this);
  CL.RemoveFromSet(&M.requirements_, this);  
  CL.Destroying(this);
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

Prohibition::Prohibition(OTuple tuple) {
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
    L1_RemoveViolation(violations_.begin()->first);    
  }
  CL.RemoveFromSet(&M.prohibitions_, this);
  CL.Destroying(this);
}
void Prohibition::Update(const QueryUpdate &update, SubType * sub){
  forall(run, update.changes_) {
    if (run->action_ == UPDATE_CREATE) {
      OTuple t = Substitute(run->data_.Data(), tuple_);
      if (exceptions_ % t) continue;
      CHECK(!(violations_ % t));
      L1_AddViolation(t);
    }
    if (run->action_ == UPDATE_DESTROY) {
      OTuple t = Substitute(run->data_.Data(), tuple_);
      if (exceptions_ % t) {
	CHECK(!(violations_ % t));
	continue;
      }
      CHECK(violations_ % t);
      L1_RemoveViolation(t);
    }
  }
}
void Prohibition::L1_AddViolation(OTuple t) {
  New<ProhibitionViolation>(this, t);
}
void Prohibition::L1_RemoveViolation(OTuple t) {
  ProhibitionViolation * violation  = violations_[t];
  CHECK(violation);
  violation->L1_Erase();
}
void Prohibition::L1_AddException(OTuple t) {
  CHECK(!(exceptions_ % t));
  CL.InsertIntoSet(&exceptions_, t);
  if (violations_ % t) L1_RemoveViolation(t);
}

