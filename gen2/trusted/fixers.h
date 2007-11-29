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

// UNTRUSTED

#ifndef _FIXERS_H_
#define _FIXERS_H_

#include "violation.h"
#include "model.h"
#include "element.h"



struct StaticExecutor {
  // running a program
  static bool Execute();
  
  // fix any violation

  static bool FixViolation(Violation *violation);

  static bool FixAllOwnedViolations(Base * owner);
  

  // fixing particular types of violations

  static bool FixMissingDynamicOn(MissingDynamicOnViolation *violation);
  static bool FixMissingOnMatch(MissingOnMatchViolation *violation);
  static bool FixExtraOnMatch(ExtraOnMatchViolation *violation);
  static bool FixChildViolation(ChildViolation *violation);
  static bool FixValue(ValueViolation *violation);
  static bool FixTime(TimeViolation *violation);
  static bool FixPost(PostViolation *violation);
  static bool FixLet(LetViolation *violation);
  static bool FixBindingOldValues(BindingOldValuesViolation *violation);

  // utilities

  static DynamicElement * 
  MakeInstantiateChild(DynamicElement *parent, int which_child);
  
  static bool FixElement(DynamicElement *e);

  static bool FixExpression(DynamicExpression *e);

  static bool FixStatement(DynamicStatement *s);

  
};

#endif
