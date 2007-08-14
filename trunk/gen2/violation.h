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
    MISSING_MULTILINK,
    MISSING_DYNAMIC_ON,
  };


  virtual void Init(OTime time);
  virtual void L1_Erase() {
    // remove from the model's set of violations
    L1_RemoveFromGlobalMap();
    CL.Destroying(this);
  }
  void L1_ChangeTime(time new_time);
  virtual ~Violation(){}
  virtual Type GetType() = 0;
  OTime GetTime() { return time_; }
  void L1_InsertIntoGlobalMap();
  void L1_RemoveFromGlobalMap();
  bool Exists();

  OTime time_;
};

struct RequirementViolation : public Violation {  
  void Init(Requirement * requirement);
  Requirement * requirement_;
  Violation::Type GetType() { return REQUIREMENT;}  
};

struct ProhibitionViolation : public Violation {
  void Init(Prohibition * prohibition, 
	    OTuple tuple, OTime time);
  Prohibition *prohibition_;
  OTuple tuple_;
  Violation::Type GetType() { return PROHIBITION;}

};

// the SingleLink has no child
class SingleLink;
struct MissingLinkViolation : public Violation {
  void Init(SingleLink * link);
  void L1_Erase();
  SingleLink *link_;
  Violation::Type GetType() { return MISSING_LINK;}
};

// the MultiLink should have a child for the given OMap, but doesn't 
struct MissingMultiLinkViolation : public Violation {
  void Init(MultiLink *link, OMap m, OTime time);
  void L1_Erase();
  MultiLink *link_;
  OMap map_;
  Violation::Type GetType() { return MISSING_MULTILINK;}
};

struct ExtraMultiLinkViolation : public Violation {
  void Init(MultiLink *link, OMap m, OTime time);
  void L1_Erase();
  MultiLink *link_;
  OMap map_;
  Violation::Type GetType() { return EXTRA_MULTILINK;}
};

// the time on a dynamic element may not be equal to its computed time.
struct TimeViolation : public Violation {
  void Init(DynamicElement *element, OTime time);
  DynamicElement *element_;
  Violation::Type GetType() { return TIME; }
};


struct MissingDynamicOnViolation :public Violation {
  void Init(OnStatement *on, OTime time);
  OnStatement *on_;
  Violation::Type GetType() { reurn MISSING_DYNAMIC_ON;}
};


#endif
