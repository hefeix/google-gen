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
#include "named.h"

class Requirement;
class Prohibition;


// base class for all violations.
struct Violation : public Named {
#define ViolationTypeList {   \
    ITEM(REQUIREMENT),	      \
      ITEM(PROHIBITION),      \
      ITEM(MISSING_LINK),			\
      ITEM(MISSING_DYNAMIC_ON),			\
      ITEM(MISSING_ON_MATCH),			\
      ITEM(EXTRA_ON_MATCH),			\
      ITEM(VALUE),				\
      ITEM(POST),				\
      ITEM(IF),				\
      ITEM(TIME),				\
      ITEM(STATIC_NO_PARENT),				\
      ITEM(DYNAMIC_NO_PARENT),				\
      ITEM(BINDING_VARIABLES),			\
      ITEM(BINDING_OLD_VALUES),			\
      ITEM(LET),				\
      };
  CLASS_ENUM_DECLARE(Violation, Type);
  
  // ---------- L2 functions ----------  

  // ---------- const functions ----------  
  virtual Named::Type GetNamedType() const { return Named::VIOLATION; }
  virtual Type GetViolationType() const = 0;
  virtual OTime ComputeTime() const = 0;
  OTime GetTime() const { return time_; }
  Record GetRecordForDisplay() const;
  bool Exists() const;
  virtual bool OwnerIsErased() const { return false;}

  // ---------- L1 functions ----------  
  virtual ~Violation(){}
  virtual void L1_Init();
  virtual void L1_Erase() {
    // remove from the model's set of violations
    L1_RemoveFromGlobalMap();
    Named::L1_Erase();
  }
  void L1_ChangeTime(OTime new_time);
  void L1_InsertIntoGlobalMap();
  void L1_RemoveFromGlobalMap();

  // ---------- N1 notifiers ----------  
  void N1_ComputedTimeChanged();

  // ---------- data ----------  
  static map<void *, set<Violation *> > owned_violations_;
  OTime time_;
  static int counts_[100];
};


// --------- const functions ----------
// Find all OwnedViolations of this type with this owner. 
set<Violation *> FindViolations(void *owner, Violation::Type type);

// returns a single violation, or null. Checks that there are at most one.
// TODO, this is inefficient in that it creates a set. 
Violation * FindViolation(void *owner, Violation::Type type);


// ---------- L1 functions ----------

// Delete all OwnedViolations with this owner
void L1_EraseOwnedViolations(void *owner);


// This is a violation that is owned by a c object called owner_.  It indexes
// itself in the global map Violation::owned_violations_.  
// To find the violation by owner and type you can call 
// FindViolations(owner, Type).  The owner when dying must call 
// EraseOwnedViolations(owner) to erase any violations it owns. 
template <class Owner, Violation::Type VType>
struct OwnedViolation : public Violation { 
  // ---------- L2 functions ----------

  // ---------- const functions ----------
  Owner *GetOwner() const { return owner_;}
  Violation::Type GetViolationType() const {return VType;}
  OTime ComputeTime() const { return owner_->GetTime();}
  Record GetRecordForDisplay() const {
    Record ret = Violation::GetRecordForDisplay();
    ret["owner"] = owner_->ShortDescription();    
    return ret;
  }
  bool OwnerIsErased() const { if (!owner_) return false;
    return owner_->IsErased();}

  // ---------- L1 functions ----------
  void L1_Init(Owner *owner) {
    owner_ = owner;
    Violation::L1_Init();
    CL.InsertIntoMapOfSets(&Violation::owned_violations_, (void *)owner_, 
			   (Violation *)this);
  }
  void L1_Erase() {
    CL.RemoveFromMapOfSets(&Violation::owned_violations_, (void *)owner_, 
			   (Violation *)this);
    Violation::L1_Erase();
  }
  static void L1_CreateIfAbsent(Owner *owner) {
    if (FindViolation(owner, VType)) return;
    New<OwnedViolation<Owner, VType> >(owner);
  }
  static void L1_RemoveIfPresent(Owner *owner) {
    Violation *v = FindViolation(owner, VType);
    if (v) v->L1_Erase();
  }
  // ---------- data ----------
  Owner *owner_;
};



// This is a violation that is owned by an owner_, and indexed by the owner
// by some data. The owner has a map from DataType to Violation *, and 
// makes it accessible to the violation by having a function
// map<DataType, Violation *> * GetViolationMap(Violation::Type);
// The owner must also L1_Erase() all of these violations upon erasing itself. 
template <class Owner, class DataType, Violation::Type VType>
struct OwnedViolationWithData : public Violation { 
  // ---------- L2 functions ----------

  // ---------- const functions ----------
  Owner *GetOwner() const { return owner_;}
  Violation::Type GetViolationType() const {return VType;}
  OTime ComputeTime() const { return owner_->GetTime();}
  bool OwnerIsErased() const { if (!owner_) return false;
    return owner_->IsErased();}

  // ---------- L1 functions ----------
  void L1_Init(Owner *owner, DataType data) {
    owner_ = owner;
    data_ = data;
    Violation::L1_Init();
    CL.InsertIntoMap(owner_->GetViolationMap(VType), data_, (Violation*)this);
  }
  void L1_Erase() {
    CL.RemoveFromMap(owner_->GetViolationMap(VType), data_);
    Violation::L1_Erase();
  }
  Record GetRecordForDisplay() const {
    Record ret = Violation::GetRecordForDisplay();
    ret["owner"] = owner_->ShortDescription();    
    ret["data"] = data_.ToString();
    return ret;
  }

  // ---------- data ----------
  Owner *owner_;
  DataType data_;
};

class StaticElement;
class DynamicElement;
class SingleLink;
class OnMultiLink;
class Element;
class StaticOn;
class DynamicExpression;
class DynamicLet;

// The static program has changed somewhere involving this node (inlcuding its
// parent link and outlinks) or one of its ancestors.  The dynamic network may
// be really messed up.  
//typedef OwnedViolation<StaticElement, Violation::STATIC_CHANGED>
//  StaticChangedViolation;


// A static node other than an ON statement has no parent.
typedef OwnedViolation<StaticElement, Violation::STATIC_NO_PARENT>
  StaticNoParentViolation;
// A dynamic node other than an ON statement has no parent.
typedef OwnedViolation<DynamicElement, Violation::DYNAMIC_NO_PARENT>
  DynamicNoParentViolation;
// A dynamic node's binding contains the wrong set of variables
// This violation is not present if there is a DynamicNoParentViolation
typedef OwnedViolation<DynamicElement, Violation::BINDING_VARIABLES>
  BindingVariablesViolation;
// A dynamic node's binding has values for the old variables that 
// differ from those of its parent
// This violation is never present if there is a BindingVariablesViolation
// or a DynamicNoParentViolation
typedef OwnedViolation<DynamicElement, Violation::BINDING_OLD_VALUES>
  BindingOldValuesViolation;
// A let statement's value doesn't match it's child's binding.
typedef OwnedViolation<DynamicLet, Violation::LET>
  LetViolation;
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
typedef OwnedViolation<Element, Violation::TIME>
  TimeViolation;
// a static on statement has no dynamic node
typedef OwnedViolation<StaticOn, Violation::MISSING_DYNAMIC_ON>
  MissingDynamicOnViolation;
// the value of a dynamic expression doesn't match its computed value. 
typedef OwnedViolation<DynamicExpression, Violation::VALUE>
  ValueViolation;
// Something is wrong with a dynamic output statement. This could be that the
// posting is missing, the posting does not match the computed tuple or the 
// computed time. The expression link could also be missing. 
// typedef OwnedViolation<OutputStatement::Dynamic, Violation::POSTING>
//  PostingViolation;
class DynamicPost;
typedef OwnedViolation<DynamicPost, Violation::POST>
  PostViolation;
class DynamicIf;
typedef OwnedViolation<DynamicIf, Violation::IF>
  IfViolation;


#endif
