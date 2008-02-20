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
      ITEM(EXECUTION),					\
      ITEM(BLACKBOARD),					\
	ITEM(ELEMENT),					\
	};
  CLASS_ENUM_DECLARE(Base, Type);

  virtual void Init(); // shadow constructor
  virtual void Erase();
  virtual ~Base() {};
 
  Object GetName() const { return name_;}
  void SetName(Object new_name);
  void AutomaticallyName();

  // This is all for output

  // Output as record format for web server usually
  virtual Record GetRecordForDisplay() const;

  // A URL that links back to a view of this object
  string GetURL() const;

  // Use GetURL and the anchortext to create a link
  string GetLink(string anchortext) const;

  // Whatever the subclass wants to add here
  virtual string TextIdentifier() const;

  // makes a link from Type + Name
  // and adds on the text identifier
  string ShortDescription() const;

  virtual Type GetBaseType() const = 0;

 private:
  Object name_;
};

class Namer { 
 public:
  Namer();
  friend class Base;
  // returns null if not found
  Base * Lookup(Base::Type type, Object name) const; 

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
