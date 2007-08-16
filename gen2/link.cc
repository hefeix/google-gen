// Copyright (C) 2007 Google Inc. and Georges Harik
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
// Author: Georges Harik and Noam Shazeer

#include "link.h"
#include "changelist.h"


Link::Init(Element *parent) {
  // Creating called at the beginning of the base class
  CL.Creating(this);
  parent_ = parent;
}

// L1_Erase is virtual and forwards all the way down
// Then is called back on the way up, destroying at the end
void Link::L1_Erase() {
  CHECK(GetChildren().size() == 0);
  EraseOwnedViolations(this);
  CL.Destroying(this);
}

OTime Link::ComputeChildTime(Element *child) const {
  return parent_->ComputeChildTime(this, child);
}


void MultiLink::L1_AddChild(Element *child){
  if (!child) return;
  CL.InsertIntoMap(&children_, child->GetBinding(), 
		   dynamic_cast<DynamicElement *>(child));
  child->L1_ConnectToParentLink(this);
}
void MultiLink::L1_RemoveChild(Element *child){
  CHECK(children_[child->GetBinding()] == child);
  CL.RemoveFromMap(&children_, child->GetBinding());
  child->L1_DisconnectFromParentLink(this);
}
set<Element *> MultiLink::GetChildren() const {
  set<Element *> ret;
  forall(run, children_) ret.insert(run->second);
  return ret;
}

void MultiLink::Init(Element * parent) {
  Link::Init(parent);
}

void SingleLink::Init(Element *parent) {
  Link::Init(parent);
  child_ = NULL;
  New<MissingLinkViolation>(this);
}

void SingleLink::L1_Erase() {
  Link::L1_Erase();
}

void SingleLink::L1_AddChild(Element *child) {
  if (!child) return;
  CHECK(!child_);
  CL.ChangeValue(&child_, child);
  child->L1_ConnectToParentLink(this);
}
void SingleLink::L1_RemoveChild(Element *child){
  CHECK(child_ == child);
  CL.ChangeValue(&child_, (Element *)NULL);
  child_->L1_DisconnectFromParentLink(this);
}
set<Element *> SingleLink::GetChildren() const {
  set<Element *> ret;
  ret.insert(child_);
  return ret;
}

void OnMultiLink::Init() {
  MultiLink::Init();
  Query * q = BB.L1_GetExecuteQuery(GetPattern(), SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT | UPDATE_WHICH | UPDATE_TIME,
			      this);
  subscription_->L1_SendCurrentAsUpdates();  
}
void OnMultiLink::Update(const QueryUpdate &update, SubType *sub) {
  forall(run, update.changes_) {
    SingleQueryUpdate s = *run;
    // if there were any bindings_ at the parent, we would have to union 
    // them here. Luckily, with an on statement, there are none. 
    OMap m = s.data_;
    if (action_ == UPDATE_CREATE) {
      if (extra_ % m) {
	ExtraMultiLinkViolation * v = extra_[m];
	v->L1_Erase();
      } else {
	New<MissingMultiLinkViolation>(this, m, s.new_time_);
      }
    }
    if (action_ == UPDATE_DESTROY) {
      if (missing_ % m) {
	missing_[m]->L1_Erase();
      } else {
	CHECK(children_ % m);
	New<ExtraMultiLinkViolation>(this, m, children_[m]->time_);
      }
    }
    if (action == UPDATE_CHANGE_TIME) {
      if (children_ % m) {
	children_[m]->L1_NewTimeShouldBe(s.new_time_);
      } else {
	CHECK(missing_ % m);
	missing_[m]->L1_ChangeTime(s.new_time_);
      }
    }
  }
}
