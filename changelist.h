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
  
#ifndef _CHANGELIST_H_
#define _CHANGELIST_H_

#include <vector>
#include <map>
#include "util.h"

typedef uint Checkpoint;
  
// Subclass this.  
// Use the constructor to make changes and the Undo() function to undo them.
// You can optionally implement the MakePermanent() function which is called
// if a change becomes permanent.  This is generally used for freeing up memory.
class Change {
 public:
  virtual void Undo() = 0;
  virtual void MakePermanent() {};
  virtual ~Change() {};
};

// Changes the value of a variable.  
// The variable must have a working copy constructor.
template <class C> class ValueChange : public Change{
 public:
  ValueChange(C * location, const C & new_val) {
    location_ = location;
    old_val_ = *location_;
    *location_ = new_val;
  }
  void Undo(){
    *location_ = old_val_;
  }
 private:
  C * location_;
  C old_val_;
};


// Inserts a value into a set.
template <class C> class SetInsertChange : public Change {
 public:
  SetInsertChange(set<C> * location, const C & val) {
    location_ = location;
    val_ = val;
    CHECK(! (*location % val_));
    location->insert(val_);
  }
  void Undo(){
    location_->erase(val_);
  }
 private:
  set<C> * location_;
  C val_;
};

// Removes a value from a set
template <class C> class SetRemoveChange : public Change {
 public:
  SetRemoveChange(set<C> * location, const C & val) {
    location_ = location;
    val_ = val;
    CHECK(*location % val_);
    location->erase(val_);
  }
  void Undo(){
    location_->insert(val_);
  }
 private:
  set<C> * location_;
  C val_;
};

// Inserts a key/value pair into a map.
// Precondition: the map must lack that key.
template <class K, class V> class MapInsertChange : public Change {
 public:
  MapInsertChange(map<K,V> * location, const K & key, const V & val) {
    location_ = location;
    key_ = key;
    CHECK(! (*location % key));
    (*location)[key] = val;
  }
  void Undo(){
    location_->erase(key_);
  }
 private:
  map<K,V> * location_;
  K key_;
};

// Removes a key/value pair from a map.
template <class K, class V> class MapRemoveChange : public Change {
 public:
  MapRemoveChange(map<K,V> * location, const K & key) {
    location_ = location;
    CHECK(*location % key);
    key_ = key;
    val_ = (*location)[key_];
    location->erase(key_);
  }
  void Undo(){
    (*location_)[key_] = val_;
  }
  map<K,V> * location_;
 private:
  K key_;
  V val_;
};

// Inserts a key/value pair into a map of sets.
// might need to create a map entry and remove it on rollback.
// MK is map key, SV is the set value type
template <class MK, class SV> class MapOfSetsInsertChange : public Change {
 public:
  MapOfSetsInsertChange(map<MK, set<SV> > * location, const MK & key, 
			    const SV & value) {
    location_ = location;
    key_ = key;
    value_ = value;
    CHECK(!((*location_)[key_] % value_));
    (*location_)[key_].insert(value_);
  }
  void Undo(){
    (*location_)[key_].erase(value_);
    if ((*location_)[key_].size()==0) location_->erase(key_);
  }
 private:
  map<MK,set<SV> > * location_;
  MK key_;
  SV value_;
};

// Removes a key/value pair into a map of sets.
// Removes the map entry when the set is empty.
// MK is map key, SV is the set value type
template <class MK, class SV> class MapOfSetsRemoveChange : public Change {
 public:
  MapOfSetsRemoveChange(map<MK, set<SV> > * location, 
			const MK & key, 
			const SV & value) {
    location_ = location;
    key_ = key;
    value_ = value;
    CHECK(((*location_)[key_] % value_));
    (*location_)[key_].erase(value_);
    if ((*location_)[key_].size()==0) location_->erase(key_);
  }
  void Undo(){
    (*location_)[key_].insert(value_);
  }
 private:
  map<MK,set<SV> > * location_;
  MK key_;
  SV value_;
};

// Adds to the value associated with a particular key.  Keys are removed
// when the associated values are 0.
template <class K, class V> class MapOfCountsAddChange : public Change {
 public:
  MapOfCountsAddChange(map<K, V> * location, const K & key, const V & delta) {
    location_ = location;
    key_ = key;
    old_value_ = (*location_)[key];
    V new_value = old_value_ + delta;
    if (new_value == 0) location_->erase(key);
    else (*location_)[key] = new_value;
  }
  void Undo(){
    if (old_value_==0) location_->erase(key_);
    else (*location_)[key_] = old_value_;
  }
 private:
  map<K,V> * location_;
  K key_;
  V old_value_;
};




// Create one of these when you new an object
template <class C> class DeleteOnRollbackChange : public Change {
 public:
  DeleteOnRollbackChange(C * object){
    object_ = object;
  }
  void Undo(){
    delete object_;
  }
 private:
  C * object_;
};

// Create one of these when you new an object
template <class C> class DeleteOnMakePermanentChange : public Change {
 public:
  DeleteOnMakePermanentChange(C * object){
    object_ = object;
  }
  void Undo(){}
  void MakePermanent(){
    delete object_;
  }
 private:
  C * object_;
};

// Takes a class instance and two void member functions with no arguments,
// and calls the first one on creation and the second one on desrtuction.
// You can also pass a MakePermanent function, or NULL.  
//
// Sample Usage:
// cl.Make(new MemberCallChange<Employee>(&fred, &Employee::Hire,
//                                              &Employee::Fire));
template <class C> class MemberCallChange : public Change {
 public:
  MemberCallChange(C * object, 
		   void (C::*change_function)(),
		   void (C::*revert_function)(),
		   void (C::*make_permanent_function)() = 0) {
    object_ = object;
    revert_function_ = revert_function;
    make_permanent_function_ = make_permanent_function;
    (object_->*change_function)();
  }
  void Undo(){
    (object_->*revert_function_)();
  }
  void MakePermanent(){
    if (make_permanent_function_) 
      (object_->*make_permanent_function_)();
  }
  void (C::*revert_function_)(); 
  void (C::*make_permanent_function_)(); 
  C * object_;
};

// same as above with one parameter functions
// Sample Usage:
// cl.Make(new MemberCall1Change<Company, Employee>
// 	  (&webvan, fred, &Company::Hire, &Company::Fire));
template <class C, class P> class MemberCall1Change : public Change {
 public:
  MemberCall1Change(C * object, P parameter, 
		   void (C::*change_function)(P),
		   void (C::*revert_function)(P),
		   void (C::*make_permanent_function)(P) = 0) {
    object_ = object;
    parameter_ = parameter;
    revert_function_ = revert_function;
    make_permanent_function_ = make_permanent_function;
    (object_->*change_function)(parameter_);
  }
  void Undo(){
    (object_->*revert_function_)(parameter_);
  }
  void MakePermanent(){
    if (make_permanent_function_) 
      (object_->*make_permanent_function_)(parameter_);
  }
  void (C::*revert_function_)(P); 
  void (C::*make_permanent_function_)(P); 
  C * object_;
  P parameter_;
};

// A changelist is a stack of changes which allows you to set checkpoints and
// rollback to desired checkpoints.  To use a changelist object, you will need
// to use either the already defined change types, or to write your own 
// subclasses of Change to do what you want.  When you subclass Change, code
// for making the change goes in the constructor, and code for rolling it 
// back goes into the destructor.  It's that simple.  
//
// When adding changes, always create them with "new" and pass them right
// to MakeChange.  For example:
// 
// Checkpoint cp = my_changelist.GetCheckpoint();
// my_changelist.Make(new MySubclassOfChange(params));
// if (UnhappyWithResults()) {
//    my_changelist.Rollback(cp);
// }

class Changelist {
 public:
  void Make(Change * c); // Make a change (use 'new' to create c inline)
  Checkpoint GetCheckpoint();  // Create a checkpoint.
  void Rollback(Checkpoint cp); // Roll back to a checkpoint.
  void MakeChangesPermanent(); // Invalidates checkpoints and frees memory.
 private:
  vector<Change *> history_;

 public:
  // convenience functions:
  template <class C> void ChangeValue(C * location, const C & new_val){
    Make(new ValueChange<C>(location, new_val));
  }
  // delete on rollback
  template <class C> void Creating(C * object){
    Make(new DeleteOnRollbackChange<C>(object));
  }
  // delete on make permanent
  template <class C> void Destroying(C * object){
    Make(new DeleteOnMakePermanentChange<C>(object));
  }

};

class DestructibleCheckpoint{
 public:
  DestructibleCheckpoint(Changelist * cl);
  ~DestructibleCheckpoint();
 private:
  Checkpoint cp_;
  Changelist *cl_;
};




#endif // _CHANGELIST_H_
