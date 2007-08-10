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

enum ViolationType{
  REQUIREMENT_VIOLATION,
  PROHIBITION_VIOLATION, 
  MISSING_LINK_VIOLATION,
  MISSING_MULTILINK_VIOLATION,
};

// base class for all violations.
struct Violation {
  virtual void Init();
  virtual void L1_Erase() {
    // remove from the model's set of violations
    L1_RemoveFromGlobalMap();
    CL.Destroying(this);
  }
  virtual ~Violation(){}
  virtual ViolationType GetType() = 0;
  Time GetTime() { return time_; }
  void L1_InsertIntoGlobalMap();
  void L1_RemoveFromGlobalMap();
  bool Exists();

  Time time_;
};

struct RequirementViolation : public Violation {  
  void Init(Requirement * requirement);
  Requirement * requirement_;
  ViolationType GetType() { return REQUIREMENT_VIOLATION;}
  
};

struct ProhibitionViolation : public Violation {
  void Init(Prohibition * prohibition, 
	    OTuple tuple);
  Prohibition *prohibition_;
  OTuple tuple_;
  ViolationType GetType() { return PROHIBITION_VIOLATION;}

};

// the SingleLink has no child
class SingleLink;
struct MissingLinkViolation : public Violation {
  void Init(SingleLink * link);
  void L1_Erase();
  SingleLink *link_;
  ViolationType GetType() { return MISSING_LINK_VIOLATION;}
};

// the MultiLink should have a child for the given OMap, but doesn't 
struct MissingMultiLinkViolation : public Violation {
  void Init(MultiLink *link, OMap m);
  void L1_Erase();
  MultiLink *link_;
  OMap map_;
  ViolationType GetType() { return MISSING_MULTILINK_VIOLATION;}
};

// the time on a dynamic element may not be equal to its computed time.
struct TimeViolation : public Violation {
  void Init(DynamicElement *element);
  DynamicElement *element_;
  ViolationType GetType() { return TIME_VIOLATION; }
};
#endif
