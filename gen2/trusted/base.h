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

class Base {
 public:
  #define BaseTypeList {				\
      ITEM(STATIC_ELEMENT),				\
	ITEM(DYNAMIC_ELEMENT),				\
	ITEM(CHOICE),					\
	ITEM(CHOOSER),						\
	ITEM(CHOOSER_SET),					\
	ITEM(REQUIREMENT),					\
	ITEM(PROHIBITION),					\
	ITEM(GIVEN),						\
	ITEM(VIOLATION),					\
	ITEM(LINK),						\
	ITEM(MODEL)						\
	};
  CLASS_ENUM_DECLARE(Base, Type);

  /*void *operator new(size_t s) {
    return GEN_ALLOC.allocate(s);
  }
  void operator delete(void *ptr) {
    return GEN_ALLOC.deallocate(ptr);
    }*/

  bool AutomaticallyName() {
    if (NameMatters()) return false;
    L1_AutomaticallyName();
    return true;
  }
  bool SetName(Object new_name) {
    if (NameMatters()) return false;
    L1_SetName(new_name);
    return true;
  }
  virtual bool NameMatters() const { return false; }

  //Base();
  void L1_Init(); // shadow constructor
  Object GetName() const { return name_;}
  void L1_SetName(Object new_name);
  void L1_AutomaticallyName();
  virtual Record GetRecordForDisplay() const;
  string GetURL() const; // link to object view
  string GetLink(string anchortext) const;
  string ShortDescription() const;
  virtual string TextIdentifier() const;
  virtual OwnedPosting * GetOwnedPosting() const { return NULL;}
  

  virtual Type GetBaseType() const = 0;
  virtual void L1_Erase();
  virtual OMap GetMap() { CHECK(false); return OMap();} // overriden for dynamic
  virtual ~Base() {};

  bool IsErased() const { return erased_;}
  bool erased_;

 private:
  Object name_;
};

class Namer { 
 public:
  Namer();
  friend class Base;
  // returns null if not found
  Base * Lookup(Base::Type type, Object name) const; 
  // a version of the previous function that returns the subtype.
  /*template <class BaseClass> 
    BaseClass * Find(Object name) const {
    return dynamic_cast<BaseClass *>(Lookup(BaseClass::GetBaseType(), name));
    }*/
  // Get a constant index of one base type by name. 
  const map<Object, Base *> & Index(Base::Type type) const 
    { return index_[type];}

  int GetCurrentCount(Base::Type t) const { return current_count_[t];}
  int GetAllTimeCount(Base::Type t) const { return all_time_count_[t];}

  void SetAutomaticallyNameAll(bool val) { automatically_name_all_ = val; }
  void SetTrackCurrentCount(bool val) { track_current_count_ = val; }
  bool AutomaticallyNameAll() const { return automatically_name_all_;}
  bool TrackCurrentCount() const { return track_current_count_;}

 private:
  vector<map<Object, Base *> > index_;
  vector<int> current_count_;
  vector<int> all_time_count_;
  int next_name_; // for automatic naming

  bool automatically_name_all_;
  bool track_current_count_;
};

extern Namer N;

#endif
