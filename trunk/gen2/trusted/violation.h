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
#include "base.h"

class Requirement;
class Prohibition;


// base class for all violations.
struct Violation : public Base {
#define ViolationTypeList {   \
    ITEM(NO_TYPE),            \
    ITEM(REQUIREMENT),	      \
      ITEM(PROHIBITION),      \
      ITEM(CHILD),			\
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
      ITEM(NEW_FLAKE),				\
      ITEM(STATIC_CHOICE),			\
      };
  CLASS_ENUM_DECLARE(Violation, Type);
  
  // ---------- L2 functions ----------  

  // ---------- const functions ----------  
  virtual Base::Type GetBaseType() const { return Base::VIOLATION; }
  virtual Type GetViolationType() const = 0;
  virtual Base * GetOwner() const { return owner_; }
  virtual OTime ComputeTime() const = 0;
  virtual bool OwnerIsErased() const { 
    CHECK(owner_); return owner_->IsErased();
  }
  OTime GetTime() const { return time_; }
  Record GetRecordForDisplay() const;
  
  // These functions search the violation index
  typedef triple<Base *, Type, Object> IndexType;
  typedef map<IndexType, Violation * > MapType;
  typedef MapType::iterator MapIteratorType;

  triple<Base *, Type, Object> GetTriple() {
    return make_triple(owner_, GetViolationType(), data_);
  }
  static Violation * 
  Search (Base * owner, Type type, Object data);
  static pair<MapIteratorType, MapIteratorType> 
    Search (Base * owner, Type type);
  static pair<MapIteratorType, MapIteratorType>
    Search (Base * owner);
  static set<Violation *>
  GetViolations(const pair<MapIteratorType, MapIteratorType> & p);

  // ---------- L1 functions ----------  

  // Deal with creation and destruction
  virtual ~Violation(){}
  virtual void L1_Init(Base * owner, Object data);
  virtual void L1_Erase();
  void L1_InsertIntoMaps();
  void L1_RemoveFromMaps();

  static void L1_EraseViolations(Base *owner) {
    set<Violation *> s = GetViolations(Search(owner));
    forall(run, s) run->L1_Erase();
  }

  // Deal with changing properties
  void L1_ChangeTime(OTime new_time);

  // ---------- N1 notifiers ----------  
  void N1_ComputedTimeChanged();

  // ---------- data ------------------
  Base *owner_;
  Object data_;
  OTime time_;
  
  static MapType violations_;
  static map<Violation::Type, set<Violation *> > violations_by_type_;
  static map<OTime, set<Violation *>, DataCompare<OTime> > violations_by_time_;

  // this counts anything ever created
  static int counts_[100];
};


template <class Owner, Violation::Type VType>
struct TypedViolation : public Violation { 
  // ---------- L2 functions ----------
    
  // ---------- const functions ----------
  Owner *GetOwner() const { return dynamic_cast<Owner *>(owner_);}
  Violation::Type GetViolationType() const {return VType;}
  // this sometimes gets overridden
  OTime ComputeTime() const { return owner_->GetTime();}

  // ---------- L1 functions ----------
  void L1_Init(Owner *owner, Object data) {
    Violation::L1_Init(owner, data);
  }
  void L1_Init(Owner *owner) { L1_Init(owner, Object(NULL));}

  void L1_Erase() {
    Violation::L1_Erase();
  }
  static TypedViolation * L1_CreateIfAbsent(Owner *owner, 
					    Object data = Object(NULL) ) {
    Violation * look = Violation::Search(owner, VType, data);
    if (look) return NULL;
    return New<TypedViolation<Owner, VType> >(owner);
  }
  static TypedViolation * L1_RemoveIfPresent(Owner *owner, 
					     Object data = Object(NULL) ) {
    Violation * look = Violation::Search(owner, VType, data);
    if (!look) return NULL;
    look->L1_Erase();
    return dynamic_cast<TypedViolation>(look);
  }
};

class StaticElement;
class DynamicElement;
class SingleLink;
class OnMultiLink;
class Element;
class StaticOn;
class DynamicExpression;
class DynamicLet;
class NewFlakeChooser;

// The static program has changed somewhere involving this node (inlcuding its
// parent link and outlinks) or one of its ancestors.  The dynamic network may
// be really messed up.  
//typedef OwnedViolation<StaticElement, Violation::STATIC_CHANGED>
//  StaticChangedViolation;


// A static node other than an ON statement has no parent.
typedef TypedViolation<StaticElement, Violation::STATIC_NO_PARENT>
  StaticNoParentViolation;
// A static node makes an impossible choice
typedef TypedViolation<StaticElement, Violation::STATIC_CHOICE>
  StaticChoiceViolation;
// A dynamic node other than an ON statement has no parent.
typedef TypedViolation<DynamicElement, Violation::DYNAMIC_NO_PARENT>
  DynamicNoParentViolation;
// A dynamic node's binding contains the wrong set of variables
// This violation is not present if there is a DynamicNoParentViolation
typedef TypedViolation<DynamicElement, Violation::BINDING_VARIABLES>
  BindingVariablesViolation;
// A dynamic node's binding has values for the old variables that 
// differ from those of its parent
// This violation is never present if there is a BindingVariablesViolation
// or a DynamicNoParentViolation
typedef TypedViolation<DynamicElement, Violation::BINDING_OLD_VALUES>
  BindingOldValuesViolation;
// A let statement's value doesn't match it's child's binding.
typedef TypedViolation<DynamicLet, Violation::LET>
  LetViolation;
// A required tuple is not present on the blackboard
typedef TypedViolation<Requirement, Violation::REQUIREMENT>
  RequirementViolation;
// a tuple on the blackboard matches a prohibited pattern
// data is an OTuple
typedef TypedViolation<Prohibition, Violation::PROHIBITION>
  ProhibitionViolation;
// an element is missing a child or has an extra child
typedef TypedViolation<Element, Violation::CHILD>
  ChildViolation;
// An on statement lacks a child for a binding which matches the blackboard.
// Data is an OMap
typedef TypedViolation<OnMultiLink, Violation::MISSING_ON_MATCH>
  MissingOnMatchViolation;
// An on statement has a child whose binding does not match the blackboard.
// Data is an OMap
typedef TypedViolation<OnMultiLink, Violation::EXTRA_ON_MATCH>
  ExtraOnMatchViolation;
// A new flake chooser generates a flake more than once. 
// data is the flake
typedef TypedViolation<NewFlakeChooser, Violation::NEW_FLAKE> NewFlakeViolation;
// the time on a dynamic element may not be equal to its computed time.
typedef TypedViolation<Element, Violation::TIME>
  TimeViolation;
// a static on statement has no dynamic node
typedef TypedViolation<StaticOn, Violation::MISSING_DYNAMIC_ON>
  MissingDynamicOnViolation;
// the value of a dynamic expression doesn't match its computed value. 
typedef TypedViolation<DynamicExpression, Violation::VALUE>
  ValueViolation;
// Something is wrong with a dynamic output statement. This could be that the
// posting is missing, the posting does not match the computed tuple or the 
// computed time. The expression link could also be missing. 
// typedef TypedViolation<OutputStatement::Dynamic, Violation::POSTING>
//  PostingViolation;
class DynamicPost;
typedef TypedViolation<DynamicPost, Violation::POST>
  PostViolation;
class DynamicIf;
typedef TypedViolation<DynamicIf, Violation::IF>
  IfViolation;


#endif
