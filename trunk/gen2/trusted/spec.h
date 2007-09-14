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

#include "extensions.h"
#include "blackboard.h"
#include "query.h"
#include "violation.h"
#include "named.h"

class Requirement : public Named {
 public:
  // ---------- L2 functions ----------
  static Requirement * Make(OTuple tuple) {
    return New<Requirement>(tuple);
  }

  // ---------- const functions ----------
  Named::Type GetNamedType() const { return Named::REQUIREMENT;}
  OTime GetTime() const { return CREATION;} // needed for violation

  // ---------- L1 functions ----------
  void L1_Init(OTuple tuple);
  void L1_Erase();
 private:
  // null if not violated
  void L1_AddViolation();
  void L1_RemoveViolation();
  typedef UpdateSubscription<QueryUpdate, Query, Requirement> SubType;
  void Update(const QueryUpdate &update, SubType * sub);

  // ---------- data ----------
  friend class UpdateSubscription<QueryUpdate, Query, Requirement>;
  SubType * subscription_;
  OTuple tuple_;
};

class Prohibition : public Named {
 public:

  // ---------- L2 functions ----------
  static Prohibition * Make(OTuple tuple) {
    return New<Prohibition>(tuple);
  }
  void AddException(OTuple t);

  // ---------- const functions ----------
  // needed for ProhibitionViolation, but shouldn't actually be called, 
  // since ProhibitionViolation has its own ComputeTime().
  Named::Type GetNamedType() const { return Named::PROHIBITION;}
  OTime GetTime() const { CHECK(false); return CREATION;} 
  // also needed by ProhibitionViolation
  map<OTuple, Violation *> * GetViolationMap(Violation::Type vtype) {
    CHECK(vtype == Violation::PROHIBITION);
    return &violations_;
  }

  // ---------- L1 functions ----------
  void L1_Init(OTuple tuple);  
  void L1_Erase();
  typedef UpdateSubscription<QueryUpdate, Query, Prohibition> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, Prohibition>;
  void Update(const QueryUpdate &update, SubType * sub);

  // ---------- data ----------  
 private:
  OTuple tuple_;
  SubType * subscription_;
  set<OTuple> exceptions_;
  map<OTuple, Violation *> violations_;
};

class Given : public Named {
 public:
  static Given * Make(OTuple tuple) {
    return New<Given>(tuple);
  }

  Named::Type GetNamedType() const { return Named::GIVEN;}

  void L1_Init(OTuple tuple);
  OwnedPosting * posting_;
  void L1_Erase();
};

void LoadSpecs(istream & input);

#endif
