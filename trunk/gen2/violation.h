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

#ifndef _VIOLATION_H_
#define _VIOLATION_H_

#include "changelist.h"
#include "objects.h"

class Requirement;
class Prohibition;


// base class for all violations.
struct Violation {
  enum Type{
    REQUIREMENT,
    PROHIBITION, 
    MISSING_LINK,
    MISSING_DYNAMIC_ON,
    MISSING_ON_MATCH,
    EXTRA_ON_MATCH,
    VALUE,
    POSTING,
    TIME,
  };

  static map<void *, set<Violation *> > owned_violations_;

  
  virtual void Init(OTime time);
  virtual void L1_Erase() {
    // remove from the model's set of violations
    L1_RemoveFromGlobalMap();
    CL.Destroying(this);
  }
  void L1_ChangeTime(OTime new_time);
  virtual ~Violation(){}
  virtual Type GetType() = 0;
  OTime GetTime() { return time_; }
  void L1_InsertIntoGlobalMap();
  void L1_RemoveFromGlobalMap();
  bool Exists();

  OTime time_;
};

// Find all OwnedViolations of this type with this owner. 
set<Violation *> FindViolations(void *owner, Violation::Type type) {
  set<Violation *> ret;
  set<Violation *> * s = Violation::owned_violations_ % owner;
  if (!s) return ret;
  forall(run, *s) if ((*run)->GetType() == type) ret.insert(*run);
  return ret;
}

// returns a single violation, or null. Checks that there are at most one.
// TODO, this is inefficient in that it creates a set. 
Violation * FindViolation(void *owner, Violation::Type type) {
  set<Violation *> s = FindViolations(owner, type);
  CHECK(s.size() <= 1);
  if (s.size()==1) return *s.begin();
  return NULL;
}

// Delete all OwnedViolations with this owner
void EraseOwnedViolations(void *owner) {
  // L1_Eraseing the violation removes it from the index, so this is a way
  // to avoid invalidating iterators. 
  while(Violation::owned_violations_ % owner) {
    (*Violation::owned_violations_[owner].begin())->L1_Erase();
  }
}


// This is a violation that is owned by a c object called owner_.  It indexes
// itself in the global map Violation::owned_violations_.  
// To find the violation by owner and type you can call 
// FindViolations(owner, Type).  The owner when dying must call 
// EraseOwnedViolations(owner) to erase any violations it owns. 
template <class Owner, Violation::Type VType>
class OwnedViolation : public Violation { 
  void Init(Owner *owner, OTime time) {
    owner_ = owner;
    Violation::Init(time);
    CL.InsertIntoMapOfSets(&Violation::owned_violations_, owner_, this);
  }
  void L1_Erase() {
    CL.RemoveFromMapOfSets(&Violation::owned_violations_, owner_, this);
  }
  Owner GetOwner() { return owner_;}
  Violation::Type GetType() const {return VType;}
  Owner *owner_;
};


// This is a violation that is owned by an owner_, and indexed by the owner
// by some data. The owner has a map from DataType to Violation *, and 
// makes it accessible to the violation by having a function
// map<DataType, Violation *> * GetViolationMap(Violation::Type);
// The owner must also L1_Erase() all of these violations upon erasing itself. 
template <class Owner, class DataType, Violation::Type VType>
class OwnedViolationWithData : public Violation { 
  void Init(Owner *owner, DataType data, OTime time) {
    owner_ = owner;
    Violation::Init(time);
    data_ = data;
    CL.InsertIntoMap(owner_->GetViolationMap(VType), data_, this);
  }
  void L1_Erase() {
    CL.RemoveFromMap(owner_->GetViolationMap(VType), data_);
  }
  Owner GetOwner() { return owner_;}
  Violation::Type GetType() const {return VType;}
  Owner *owner_;
  DataType data_;
};

class SingleLink;
class OnMultiLink;
class DynamicElement;
class OnStatement;
class DynamicExpression;
class OutputStatement;

// A required tuple is not present on the blackboard
typedef OwnedViolation<Requirement, Violation::REQUIREMENT>
  RequirementViolation;
// a tuple on the blackboard matches a prohibited pattern
typedef OwnedViolationWithData<Prohibition, OTuple, Violation::PROHIBITION>
  ProhibitionViolation;
// a SingleLink has no child.
typedef OwnedViolation<SingleLink, Violation::MISSING_LINK>
  MissingLinkViolation;
// An on statement lacks a child for a binding which matches the blackboard.
typedef OwnedViolationWithData<OnMultiLink, OMap, Violation::MISSING_ON_MATCH>
  MissingOnMatchViolation;
// An on statement has a child whose binding does not match the blackboard.
typedef OwnedViolationWithData<OnMultiLink, OMap, Violation::EXTRA_ON_MATCH>
  ExtraOnMatchViolation;
// the time on a dynamic element may not be equal to its computed time.
typedef OwnedViolation<DynamicElement, Violation::TIME>
  TimeViolation;
// a static on statement has no dynamic node
typedef OwnedViolation<OnStatement, Violation::MISSING_DYNAMIC_ON>
  MissingDynamicOnViolation;
// the value of a dynamic expression doesn't match its computed value. 
typedef OwnedViolation<DynamicExpression, Violation::VALUE>
  ValueViolation;

// This one is actually declared in element.h, since we can't forward declare
// OutputStatement::Dynamic :(
// Something is wrong with a dynamic output statement. This could be that the
// posting is missing, the posting does not match the computed tuple or the 
// computed time. The expression link could also be missing. 
// typedef OwnedViolation<OutputStatement::Dynamic, Violation::POSTING>
//  PostingViolation;


#endif
