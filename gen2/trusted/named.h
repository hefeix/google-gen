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

#ifndef _NAMED_H_
#define _NAMED_H_

#include "objects.h"
#include "record.h"

class OwnedPosting;

class Named {
 public:
  #define NamedTypeList {				\
      ITEM(STATEMENT),					\
	ITEM(EXPRESSION),				\
	ITEM(DYNAMIC_STATEMENT),			\
	ITEM(DYNAMIC_EXPRESSION),			\
	ITEM(CHOOSER),					\
	ITEM(REQUIREMENT),				\
	ITEM(PROHIBITION),				\
	ITEM(GIVEN),					\
	ITEM(VIOLATION),					\
	ITEM(LINK),						\
	ITEM(MODEL)					\
	};
  CLASS_ENUM_DECLARE(Named, Type);

  
  //Named();
  void L1_Init(); // shadow constructor
  Object GetName() const { return name_;}
  void L1_SetName(Object new_name_);
  void L1_AutomaticallyName();
  virtual Record GetRecordForDisplay() const;
  string GetURL() const; // link to object view
  bool IsErased() const { return erased_;}
  string GetLink(string anchortext) const;
  string ShortDescription() const;
  virtual string TextIdentifier() const;
  virtual OwnedPosting * GetOwnedPosting() const { return NULL;}


  virtual Type GetNamedType() const = 0;
  virtual void L1_Erase();
  virtual OMap GetMap() { CHECK(false); return OMap();} // overriden for dynamic
  virtual ~Named() {};

  static int counts_[100]; // for debugging purposes
 private:
  Object name_;
  bool erased_;
};

class Namer {
 public:
  Namer();
  friend class Named;
  // returns null if not found
  Named * Lookup(Named::Type type, Object name) const; 
  // a version of the previous function that returns the subtype.
  template <class NamedClass> 
    NamedClass * Find(Object name) const {
    return dynamic_cast<NamedClass *>(Lookup(NamedClass::Type(), name));
  }
  // Get a constant index of one named type by name. 
  const map<Object, Named *> & Index(Named::Type type) const 
    { return index_[type];}

 private:
  vector<map<Object, Named *> > index_;
  int next_name_; // for automatic naming
};

extern Namer N;

#endif
