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
#include "element.h"
#include "changelist.h"
#include "webserver.h"
#include "options.h"

void Link::L1_Init(Element *parent) {
  // Creating called at the beginning of the base class
  Named::L1_Init();
  parent_ = parent;
}

// L1_Erase is virtual and forwards all the way down
// Then is called back on the way up, destroying at the end
void Link::L1_Erase() {
  CHECK(GetChildren().size() == 0);
  Named::L1_Erase();
}

// needed by various violations.
OTime Link::GetTime() const {
  return GetParent()->GetTime();
}

OTime Link::ComputeChildTime(const Element *child) const {
  return parent_->ComputeChildTime(this, child);
}
int Link::WhichChildAmI() const { 
  Named::Type pt = GetParent()->GetNamedType();
  if (pt == Named::STATEMENT
      || pt == Named::EXPRESSION) {
    StaticElement *parent = dynamic_cast<StaticElement *>(GetParent());
    for (uint i=0; i<parent->static_children_.size(); i++) 
      if (this == parent->static_children_[i]) return i;
  }
  if (pt == Named::DYNAMIC_STATEMENT
      || pt == Named::DYNAMIC_EXPRESSION) {
    DynamicElement *parent = dynamic_cast<DynamicElement *>(GetParent());
    for (uint i=0; i<parent->children_.size(); i++) 
      if (this == parent->children_[i]) return i;
  }
  return -1;
}
string Link::TextIdentifier() const {
  return "from " + GetParent()->ShortDescription();
}
Record Link::GetRecordForDisplay() const { 
  Record ret = Named::GetRecordForDisplay();
  ret["parent"] = GetParent()->ShortDescription();
  ret["children"] = ChildListings();
  return ret;
}
string Link::ChildListings(int max_children) const{
  string ret;
  set<Element *> children = GetChildren();
  int count = 0;
  forall(run, children) {
    if (count++ == max_children) {
      ret += "... " + 
	HTMLLink(GetURL(), "(" + itoa(children.size()) + " total)")
	+ "<br>\n";
      
      break;
    }
    ret += (*run)->ShortDescription() + "<br>\n";
  }
  return ret;
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
  forall(run, children_) {
    CHECK(run->second);
    ret.insert(run->second);
  }
  return ret;
}
Element * MultiLink::GetChild(OMap m) const {
  DynamicElement *const * look = children_ % m;
  if (look) return *look;
  return NULL;
}

void MultiLink::L1_Init(Element * parent) {
  Link::L1_Init(parent);
}

void SingleLink::L1_Init(Element *parent) {
  Link::L1_Init(parent);
  child_ = NULL;
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
  child_->L1_DisconnectFromParentLink(this);
  CL.ChangeValue(&child_, (Element *)NULL);
}
set<Element *> SingleLink::GetChildren() const {
  set<Element *> ret;
  if (child_) ret.insert(child_);
  return ret;
}

DynamicOn * OnMultiLink::GetDynamicOnParent() const {
  return dynamic_cast<DynamicOn *>(GetParent());
}

void OnMultiLink::L1_Init(DynamicOn *parent) {
  MultiLink::L1_Init(parent);
  Query * q = BB.L1_GetExecuteQuery(GetDynamicOnParent()->GetPattern(), 
				    SamplingInfo(), NULL);
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
    if (s.action_ == UPDATE_CREATE) {
      if (extra_ % m) {
	ExtraOnMatchViolation * v 
	  = dynamic_cast<ExtraOnMatchViolation*>(extra_[m]);
	v->L1_Erase();
      } else {
	New<MissingOnMatchViolation>(this, m);
      }
    }
    if (s.action_ == UPDATE_DESTROY) {
      if (missing_ % m) {
	missing_[m]->L1_Erase();
      } else {
	CHECK(children_ % m);
	New<ExtraOnMatchViolation>(this, m);
      }
    }
    if (s.action_ == UPDATE_CHANGE_TIME) {
      if (children_ % m) {
	// Note - We could use max(parent_->time_, s.new_time_)
	// instead of computing it again. That would be an optimization
	children_[m]->N1_StoredOrComputedTimeChanged();
      } else {
	CHECK(missing_ % m);	
	// if we want to optimize ... use the time from the update.
	missing_[m]->N1_ComputedTimeChanged();
      }
    }
  }
}

OnMultiLink::~OnMultiLink() {}

void OnMultiLink::L1_AddChild(Element * child) {
  // L1_Erase leaves the bindings anyways when called on the child
  OMap binding = child->GetBinding();
  if (missing_ % binding) {
    missing_[binding]->L1_Erase();
  } else {
    New<ExtraOnMatchViolation>(this, binding);
  }
  MultiLink::L1_AddChild(child);
}

void OnMultiLink::L1_RemoveChild(Element * child) {
  OMap binding = child->GetBinding();
  if (extra_ % binding) {
    extra_[binding]->L1_Erase();
  } else {
    New<MissingOnMatchViolation>(this, binding);
  }
  MultiLink::L1_RemoveChild(child);
}
