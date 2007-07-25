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

#ifndef _NAMER_H_
#define _NAMER_H_

#include "objects.h"

enum NamedType {
  STATEMENT,
  EXPRESSION,
  DYNAMIC_STATEMENT,
  DYNAMIC_EXPRESSION,
  CHOOSER,
  NUM_NAMED_TYPES,
};

class Named {
 public:
  Named();
  Object Name() const;
  void L1_SetName(Object new_name_);
  void L1_AutomaticallyName();
  virtual NamedType Type() const = 0;
  virtual void L1_Erase();
  virtual OMap GetMap() { CHECK(false);} // overriden for dynamic nodes.
  virtual ~Named() {};
 private:
  Object name_;
};

class Namer {
 public:
  Namer();
  friend class Named;
  // returns null if not found
  Named * Lookup(NamedType type, Object name) const; 
  // a version of the previous function that returns the subtype.
  template <class NamedClass> 
    NamedClass * Find(Object name) const {
    return dynamic_cast<NamedClass *>(Lookup(NamedClass::Type(), name));
  }
  // Get a constant index of one named type by name. 
  const map<Object, Named *> & Index(NamedType type) const 
    { return index_[type];}

 private:
  vector<map<Object, Named *> > index_;
  int next_name_; // for automatic naming
};

extern Namer N;

#endif
