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

#include "element.h"
#include "changelist.h"

Link::Link(Element *parent) {
  CL.Creating(this);
  parent_ = parent;
}
void Link::L1_Erase() {
  CHECK(GetChildren().size() == 0);
  CL.Destroying(this);
}
void MultiLink::L1_AddChild(Element *child){
  if (!child) return;
  CL.InsertIntoMap(&children_, child->GetMap(), child);
  child->L1_ConnectToParentLink(this);
}
void MultiLink::L1_RemoveChild(Element *child){
  CHECK(children_[child->GetMap()] == child);
  CL.RemoveFromMap(&children_, child->GetMap());  
  child->L1_DisconnectFromParentLink(this);
}
set<Element *> MultiLink::GetChildren() const {
  set<Element *> ret;
  forall(run, children_) ret.insert(run->second);
  return ret;
}

SinSingleLink::SingleLink(Element *parent) :
  Link(parent), child_(NULL), violation_(new MissingLinkViolation(this)){
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
