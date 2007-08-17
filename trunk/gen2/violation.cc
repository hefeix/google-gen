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

map<void *, set<Violation *> > Violation::owned_violations_;

void Violation::Init(OTime time) {  
  CL.Creating(this);
  time_ = time;
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
void Violation::L1_ChangeTime(OTime new_time) {
  CL.RemoveFromMapOfSets(&M.violations_by_time_, GetTime(), this);
  CL.ChangeValue(&time_, new_time);
  CL.InsertIntoMapOfSets(&M.violations_by_time_, GetTime(), this);
}

bool Violation::Exists() {
  if (!(M.violations_by_time_ % time_)) return false;
  if (M.violations_by_time_[time_] % this) return true;
  return false;
}

