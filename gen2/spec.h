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

// A prohibition says that tuples matching a particular tuple with wildcards.
// are forbidden except for a list of exceptions.   
// Wildcards are represented by $0, but multiple wildcards can match different
// literals.
// Creating prohibitions causes the model to automatically track their 
// violations.  Prohibitions can be violated at layer 2 (local consistency) 
// but not at layer 3 (global consistency).

#ifndef _SPEC_H_
#define _SPEC_H_

#include "blackboard.h"
#include "query.h"

class RequirementViolation;
class ProhibitionViolation;

class Requirement {
 public:
  Requirement(OTuple tuple);  
  void L1_Erase();
  typedef UpdateSubscription<QueryUpdate, Query, Requirement> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, Requirement>;

 private:
  // null if not violated
  void L1_AddViolation();
  void L1_RemoveViolation();
  void Update(const QueryUpdate &update, SubType * sub);
  SubType * subscription_;
  OTuple tuple_;
};

class Prohibition {
 public:
  Prohibition(OTuple tuple);  
  void L1_Erase();
  void L1_AddException(OTuple t);
  map<OTuple, Violation *> * GetViolationMap(Violation::Type vtype) {
    CHECK(vtype == Violation::PROHIBITION);
    return &violations_;
  }
  
  typedef UpdateSubscription<QueryUpdate, Query, Prohibition> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, Prohibition>;
 private:
  OTuple tuple_;
  SubType * subscription_;
  set<OTuple> exceptions_;
  map<OTuple, Violation *> violations_;
  void Update(const QueryUpdate &update, SubType * sub);
  void L1_AddViolation(OTuple t);
  void L1_RemoveViolation(OTuple t);
};

#endif
