/ Copyright (C) 2007 Google Inc. and Georges Harik
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
  CL.Destroying(this);
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

void Multilink::Init(Element * parent) {
  Link::Init(parent);
}

void SingleLink::Init(Element *parent) {
  Link::Init(parent);
  child_ = NULL;
  violation_ = new MissingLinkViolation(this);
}

void SingleLink::L1_Erase() {
  violation_->L1_Erase();
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

