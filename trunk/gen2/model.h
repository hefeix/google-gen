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

#ifndef _MODEL_H_
#define _MODEL_H_

#include "blackboard.h"
#include "static.h"

class Named {
  Object name_;
  void L1_SetName(Object new_name_);
  void L1_AutomaticallyName();
  virtual void L1_Erase();
  Named();
};

class Model {
 public:
  Model();

  // Reading things in from an object
  void LoadFromObject(Object o);
  Object WriteToObject() const;

  // The problem specification
  set<Requirement *> requirements_;
  set<Prohibition *> prohibitions_;

  // The static statements
  set<Statement *> static_statements_;  

  // The dynamic statements

  // The violations
  map<ViolationType, set<Violation *> > violations_;

  // The ln likelihood
  LL ln_likelihood_;

  // All Choosers
  set<Chooser *> all_choosers_;

  // Global Choosers
  Chooser * global_flake_chooser_;
  UintChooser * global_uint_chooser_;

  // all named objects by name
  map<Object, Named *> name_index_;
  int next_name_;  
};


extern Model M;

#endif
