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

#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include "util.h"

enum ObjectType {
  FLAKE,
  VARIABLE,
  TUPLE,
  BOOLEAN,
  INTEGER,
  REAL,
  ESCAPE,
  ERRORTYPE,
};

// There will exist no two identical ObjectDefinition objects.
struct ObjectDefinition {
  virtual ObjectType type() const = 0;
  virtual string ToString() const {
    return "ERROR";
  }
  int reference_count_;
  ObjectDefinition() {
    reference_count_ = 0;
  }
};

class Object {
 public:
  Object() {
    def_ = NULL;
  }
  Object(ObjectDefinition * def) {
    def_ = NULL;
    PointTo(def);
  };
  Object & operator =(const Object &o) {
    PointTo(o.def_);
    return *this;
  }
  Object(const Object & o) {
    def_ = NULL;
    *this = o;
  }
  ~Object() {
    PointTo(NULL);
  };
  ObjectType type() const { 
    return def_->type();
  }
  const ObjectDefinition * GetObjectDefinition() const {
    return def_;
  }
  string ToString() const 
  { if (def_) return def_->ToString(); return "__NULL__"; }

 private:
  ObjectDefinition * def_;
  void PointTo(ObjectDefinition *def){
    if (def_) {
      def_->reference_count_--;
      if (def_->reference_count_==0) delete def_;
    }
    def_ = def;
    if (def_) {
      def_->reference_count_++;
    }
  }
};

bool operator ==(const Object & o1, const Object & o2) {
  return (o1.GetObjectDefinition()==o2.GetObjectDefinition());
}
bool operator <(const Object & o1, const Object & o2) {
  return (o1.GetObjectDefinition()<o2.GetObjectDefinition());
}

template <ObjectType OT, class D>
class SpecificDefinition : public ObjectDefinition {
 public:
  D data_;
  SpecificDefinition(D data) {
    data_ = data;
    CHECK(!(unique_ % data));
    unique_[data_] = this;
  }
  
  ~SpecificDefinition() {
    unique_.erase(data_);
  }
  ObjectType type() const { 
    return OT;
  }
  string ToString() const;
  static map<D, SpecificDefinition<OT, D> *> unique_;
};

template <ObjectType OT, class D>
class SpecificObject : public Object {
 public:
  SpecificObject(const Object & o) : Object(o) {
    CHECK(o.type() == OT);    
  }
    SpecificObject(ObjectDefinition *def) 
      : Object(def) {
      CHECK(def->type() == OT);
    }
      static SpecificObject<OT, D> Make(D data) {
	SpecificDefinition<OT, D> ** find = SpecificDefinition<OT, D>::unique_ % data;
	if (find) {
	  return SpecificObject<OT,D>(*find);
	}
	SpecificDefinition<OT, D> * new_def 
	  = new SpecificDefinition<OT, D>(data);
	return SpecificObject<OT, D>(new_def);
      }
      
      const SpecificDefinition<OT, D> * GetDefinition(){
	return dynamic_cast<const SpecificDefinition<OT, D> *>
	  (GetObjectDefinition());
      }
};


typedef SpecificDefinition<FLAKE, string> FlakeDefinition;
typedef SpecificObject<FLAKE, string> Flake;

typedef SpecificDefinition<VARIABLE, int> VariableDefinition;
typedef SpecificObject<VARIABLE, int> Variable;

typedef SpecificDefinition<TUPLE, vector<Object> > TupleDefinition;
typedef SpecificObject<TUPLE, vector<Object> > Tuple;

typedef SpecificDefinition<BOOLEAN, bool> BooleanDefinition;
typedef SpecificObject<BOOLEAN, bool> Boolean;

typedef SpecificDefinition<INTEGER, int> IntegerDefinition;
typedef SpecificObject<INTEGER, int> Integer;

typedef SpecificDefinition<REAL, double> RealDefinition;
typedef SpecificObject<REAL, double> Real;

typedef SpecificDefinition<ESCAPE, Object> EscapeDefinition;
typedef SpecificObject<ESCAPE, Object> Escape;




#endif

