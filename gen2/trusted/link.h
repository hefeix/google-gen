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

#ifndef _LINK_H_
#define _LINK_H_

#include "query.h"
#include "violation.h"

class Element;
class DynamicElement;
class StaticElement;

// we use the convention that parents own all links.
struct Link : public Named {
  enum Type{
    SINGLE,
    MULTI,
    ON,
  };

  // ---------- L2 functions ----------

  // ---------- const functions ----------
  Named::Type GetNamedType() const { return Named::LINK; }
  Element * GetParent() const { return parent_;}
  // needed by various violations
  OTime GetTime() const;
  virtual set<Element *> GetChildren() const = 0;
  // assumes this is the parent_ link of the child.
  OTime ComputeChildTime(const Element *child) const;
  int WhichChildAmI() const;
  string TextIdentifier() const;
  string ChildListings(int max_children = -1) const;
  Record GetRecordForDisplay() const;

  // ---------- L1 functions ----------  
  // Init & Erase
  virtual void L1_Init(Element * parent);
  virtual void L1_Erase();

  // Modifiers
  virtual void L1_AddChild(Element *child) = 0;
  virtual void L1_RemoveChild(Element *child) = 0;
  // Just to make the compiler happy
  virtual ~Link() {}

  // ---------- data ----------  
  Element * parent_;
};
struct MultiLink : public Link {
  // Init & Erase
  void L1_Init(Element *parent);
  void L1_Erase() { Link::L1_Erase();}
  
  // Modifiers
  void L1_AddChild(Element *child);
  void L1_RemoveChild(Element *child);

  // Accessors
  Element * GetChild(OMap m) const;
  set<Element *> GetChildren() const;

  // To make the compiler happy
  virtual ~MultiLink(){}

  // Data
  map<OMap, DynamicElement *> children_;
};

class DynamicOn;

// this is a multilink where the children should exactly match the
// satisfactions of a pattern.  Violations are created automatically. 
struct OnMultiLink : public MultiLink {
  // Init and Erase
  void L1_Init(DynamicOn *parent);
  void L1_Erase() { MultiLink::L1_Erase();}
  ~OnMultiLink();

  // Accessors
  // casts GetParent() to a DynamicOn
  DynamicOn * GetDynamicOnParent() const;

  void L1_AddChild(Element *child);
  void L1_RemoveChild(Element *child);

  typedef UpdateSubscription<QueryUpdate, Query, OnMultiLink> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, OnMultiLink>;
  void Update(const QueryUpdate &update, SubType *sub);
  map<OMap, Violation *> * GetViolationMap(Violation::Type vtype) {
    if (vtype == Violation::MISSING_ON_MATCH) return &missing_;
    if (vtype == Violation::EXTRA_ON_MATCH) return &extra_;
    CHECK(false);
    return NULL;
  }
  map<OMap, Violation *> missing_;
  map<OMap, Violation *> extra_;
  SubType * subscription_;
};

struct SingleLink : public Link {
  // Init & Erase
  void L1_Init(Element *parent);
  void L1_Erase();

  // Modifiers
  void L1_AddChild(Element *child);
  void L1_RemoveChild(Element *child);
  void L1_SetOptional(bool optional);
  void L1_CheckSetMissingLinkViolation();

  // Accessors
  Element * GetChild() const { return child_;  }
  set<Element *> GetChildren() const;

  // For compiler warnings
  virtual ~SingleLink(){}

  // Data
  Element * child_;
  bool optional_;
};

#endif
