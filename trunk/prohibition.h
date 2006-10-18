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

#ifndef _PROHIBITION_H_
#define _PROHIBITION_H_

#include "component.h"

class Prohibition {
  // I have no friends.  Just use the public methods and we will be
  // acquaintances.  
 public:
  // Factory constructor to adhere to the L1 naming convention and to
  // make sure that Prohibitions are only created using new.  
  static Prohibition * L1_MakeProhibition(Model *m, Tuple prohibited);
  void L1_Erase();
  // Adds the exception and possibly removes a violation
  void L1_AddException(Tuple exception);
  // Removes the exception and possibly adds a violation
  void L1_RemoveException(Tuple exception);
  // We call this function when a new TrueTuple is created and matches the
  // prohibited pattern.  We check whether it is an exception, and if it
  // isn't, add it as a violation.
  void L1_CheckAddViolation(TrueTuple *t);

 private:

  // ----- CONSTRUCTORS -----

  Prohibition(Model *m, Tuple prohibited);


  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  // These are not just accessor functions.  They also add/remove links to the
  // prohibition from the violating TrueTuple, and from the model's set
  // of violated prohibitions.
  void L1_AddViolation(TrueTuple *t);
  void L1_RemoveViolation(TrueTuple *t);

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_SetExists(bool val);
  void A1_AddException(Tuple exception);
  void A1_RemoveException(Tuple exception);
  void A1_AddViolation(TrueTuple *t);
  void A1_RemoveViolation(TrueTuple *t);


  // ----- DATA -----

  bool exists_;
  Tuple prohibited_;
  set<Tuple> exceptions_;
  set<TrueTuple *> violations_;
  Model * model_;
};


#endif
