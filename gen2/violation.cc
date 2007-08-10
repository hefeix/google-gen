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
// Author: Noam Shazeer and Georges Harik

#include "violation.h"
#include "model.h"
#include "changelist.h"
#include "element.h"

void Violation::Init() {  
  CL.Creating(this);
  // add to the model's set of violations
  L1_InsertIntoGlobalMap();
}
void Violation::L1_InsertIntoGlobalMap(){
  CL.InsertIntoMapOfSets(&M.violations_by_type_, GetType(), this);
  CL.InsertIntoMapOfSets(&M.violations_by_time_, GetTime(), this);
}
void Violation::L1_RemoveFromGlobalMap(){
  CL.RemoveFromMapOfSets(&M.violations_by_type_, GetType(), this);
  CL.RemoveFromMapOfSets(&M.violations_by_time_, GetTime(), this);
}

bool Violation::Exists() {
  if (!M.violations_by_time_ % time_) return false;
  if (M.violations_by_time_[time_] % this) return true;
  return false;
}

void RequirementViolation::Init(Requirement *requirement) {
  Violation::Init();
  requirement_ = requirement;
  CL.ChangeValue(&requirement_->violation_, this);
}
void RequirementViolation::L1_Erase() {
  CL.ChangeValue(&requirement_->violation_, NULL);
  Violation::L1_Erase();
}
void ProhibitionViolation::Init(Prohibition *prohibition,
				OTuple tuple) {
  Violation::Init();
  prohibition_ = prohibition;
  tuple_ = tuple;
  CL.InsertIntoMap(&prohibition_->violations_, tuple_, this);
}
void ProhibitionViolation::L1_Erase(){
  CL.RemoveFromMap(&prohibition_->violations_, tuple_);
}
void MissingLinkViolation::Init(SingleLink *link) {
  Violation::Init();
  link_ = link;
  CHECK(link_->violation_ == NULL);
  CL.ChangeValue(link_->violation_, this);
}
MissingLinkViolation::L1_Erase() {
  CHECK(link_->violation_ == this);
  CL.ChangeValue(link_->violation_, NULL);
  Violation::L1_Erase();
}
void MissingMultiLinkViolation::Init(MultiLink *link, OMap m) {
  Violation::Init();
  link_ = link;
  map_ = m;
  CL.InsertIntoMap(&link_->violations_, m, this);  
}
void MissingMultiLinkViolation::L1_Erase() {
  CL.RemoveFromMap(&link_->violations_, m);
  Violation::L1_Erase();
}
void TimeViolation::Init(DynamicElement *element) {
  element_ = element;
  CHECK(element_->time_violation_ == NULL);
  CL.ChangeValue(&element_->time_violation_, this);
}
void TimeViolation::L1_Erase() {
  CL.ChangeValue(&element_->time_violation_, NULL);
  Violation::L1_Erase();
}
void BindingViolation::Init(DynamicElement *element) {
  element_ = element;
  CHECK(element_->binding_violation_ == NULL);
  CL.ChangeValue(&element_->binding_violation_, this);
}
void BindingViolation::L1_Erase() {
  CL.ChangeValue(&element_->binding_violation_, NULL);
  Violation::L1_Erase();
}

