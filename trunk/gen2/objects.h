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
#include "hash.h"
#include "numbers.h"

enum ObjectType {
  OBJECT,
  FLAKE,
  KEYWORD,
  VARIABLE,
  OTUPLE,
  OMAP,
  OPATTERN,
  BOOLEAN,
  INTEGER,
  REAL,
  OTIME, 
  OBITSEQ,
  ESCAPE,
  ERRORTYPE,
};
inline string ObjectTypeName(ObjectType t) {
  switch(t) {
  case FLAKE: return "FLAKE";
  case KEYWORD: return "KEYWORD";
  case VARIABLE: return "VARIABLE";
  case OTUPLE: return "OTUPLE";
  case OMAP: return "OMAP";
  case OPATTERN: return "OPATTERN";
  case BOOLEAN: return "BOOLEAN";
  case INTEGER: return "INTEGER";
  case REAL: return "REAL";
  case OTIME: return "OTIME";
  case OBITSEQ: return "OBITSEQ";
  case ESCAPE: return "ESCAPE";
  case ERRORTYPE: return "ERRORTYPE";
  default: CHECK(false); return "ERROR";
  }
}

// There will exist no two identical ObjectDefinition objects.
struct ObjectDefinition {
  virtual ObjectType Type() const = 0;
  string ToString(bool verbose = false) const {
    string ret = ToStringSpecific(verbose);
    if (verbose) ret = "<" + ObjectTypeName(Type()) + " rc=" 
      + itoa(reference_count_) + "> " + ret;
    return ret;
  }
  virtual string ToStringSpecific(bool verbose) const {
    return "ERROR";
  }
  virtual ~ObjectDefinition(){}
  int reference_count_;
  ObjectDefinition() {
    reference_count_ = 0;
  }
  virtual uint64 DeepFingerprint(uint64 level = 0) const = 0;
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
  Object(void *null) {
    CHECK(null==NULL);
    def_ = NULL;
  }
  Object & operator =(const Object &o) {
    PointTo(o.def_);
    return *this;
  }
  void * operator =(void *null){
    CHECK(null==NULL);
    PointTo(NULL);
    return NULL;
  }
  Object(const Object & o) {
    def_ = NULL;
    *this = o;
  }
  virtual ~Object() {
    PointTo(NULL);
  };
  // returns the type of the definition (which is always a subtype)
  // should only be called on a generic object. 
  ObjectType Type() const { 
    CHECK(def_ != NULL);
    return def_->Type();
  }
  // returns the type of this Object object
  virtual ObjectType ReferenceType() const{
    return OBJECT;
  }
  const ObjectDefinition * GetObjectDefinition() const {
    return def_;
  }
  string ToString(bool verbose=false) const 
  { if (def_) return def_->ToString(verbose); return "null"; }
  uint64 ShallowFingerprint(uint64 level = 0) const {
    return ::Fingerprint((uint64)def_, level);
  }
  uint64 DeepFingerprint(uint64 level = 0) const {
    return def_->DeepFingerprint(level);
  }

 private:
  ObjectDefinition * def_;
  void PointTo(ObjectDefinition *def){
    if (def_) {
      def_->reference_count_--;
      if (def_->reference_count_==0) delete def_;
    }
    def_ = def;
    if (def_) {
      // run-time type checking.
      CHECK(ReferenceType()==OBJECT || ReferenceType() == def_->Type());
      def_->reference_count_++;
    }
  }
};


inline bool operator ==(const Object & o1, const Object & o2) {
  return (o1.GetObjectDefinition()==o2.GetObjectDefinition());
}
inline bool operator <(const Object & o1, const Object & o2) {
  return (o1.GetObjectDefinition()<o2.GetObjectDefinition());
}
inline bool operator !=(const Object &o1, const Object &o2){
  return !(o1==o2);
}

// TODO: we could make some types of objects that own their definitions.  
// We would have to change object comparison to first be by type.  

template <ObjectType OT, class D>
class SpecificObject : public Object {
 public:
  class Definition : public ObjectDefinition 
  {
  public:
    D data_;
    Definition(D data) {
      data_ = data;
      CHECK(!(unique_ % data));
      unique_[data_] = this;
    }
    
    ~Definition() {
      unique_.erase(data_);
    }
    ObjectType Type() const { 
      return OT;
    }
    string ToStringSpecific(bool verbose) const;
    const D & Data() const { return data_; }
    uint64 DeepFingerprint(uint64 level = 0) const{
      return ::Fingerprint(::Fingerprint(data_, OT), level); 
    }

  };
  static map<D, Definition *> unique_;


  // Make a SpecificObject from data.
  // We can't just make this a constructor, because it conflicts with the copy
  // constructor for Escapes
  static SpecificObject<OT, D> Make(D data) {
    Definition ** find = unique_ % data;
    if (find) {
      return SpecificObject<OT,D>(*find);
    }
    Definition * new_def = new Definition(data);
    return SpecificObject<OT, D>(new_def);
  }
  ObjectType ReferenceType() const{
    return OT;
  }
  
  const Definition * GetDefinition() const{
    return dynamic_cast<const Definition *>(GetObjectDefinition());
  }

  const D & Data() const{ return GetDefinition()->Data();}
  //  const D & operator *() const{return Data();}
  
  // this should work for tuples.  I guess it doesn't cause compile errors
  // for the rest because it's in a template.
  Object operator [](int i) { return Data()[i]; }
  uint size() { return Data().size(); }

  // constructors at bottom to prevent emacs from @#($*@ing up the indentation
  SpecificObject() : Object() {}
    
    SpecificObject(const Object & o) : Object(o) {
      CHECK(o.Type() == OT);    
    }
      
      SpecificObject(ObjectDefinition *def) 
	: Object(def) {
	CHECK(def->Type() == OT);
      }
	
	SpecificObject(void *null) : Object(null) {}
};

istream & operator >>(istream & input, Object & o);

inline ostream & operator <<(ostream & output, const Object & o) {
  output << o.ToString();
  return output;
}

typedef vector<Object> Tuple;
typedef map<Object, Object> Map;
typedef vector<Tuple> MPattern; // mutable 2 levels down


typedef SpecificObject<FLAKE, string> Flake;
typedef SpecificObject<KEYWORD, string> Keyword;
typedef SpecificObject<VARIABLE, int> Variable;
typedef SpecificObject<OTUPLE, Tuple > OTuple;
typedef SpecificObject<OMAP, Map> OMap;
typedef SpecificObject<BOOLEAN, bool> Boolean;
typedef SpecificObject<INTEGER, int> Integer;
typedef SpecificObject<REAL, double> Real;
typedef SpecificObject<OTIME, Time> OTime;
typedef vector<OTuple> Pattern;
typedef SpecificObject<OPATTERN, Pattern> OPattern;
typedef SpecificObject<OBITSEQ, BitSeq> OBitSeq;
typedef SpecificObject<ESCAPE, Object> Escape;

inline const Object * operator %(const OMap & m, Object key) {
  return m.Data() % key;
}

template <> 
inline uint64 OTuple::Definition::DeepFingerprint(uint64 level) const {
  uint64 ret = ::Fingerprint(OTUPLE, level);
  forall(run, data_) ret = run->DeepFingerprint(ret);
  return ret;
}
template <> 
inline uint64 OMap::Definition::DeepFingerprint(uint64 level) const {
  uint64 ret = ::Fingerprint(OMAP, level);
  forall(run, data_) {
    ret = run->first.DeepFingerprint(ret);
    ret = run->second.DeepFingerprint(ret);
  }
  return ret;
}
template <> 
inline uint64 Escape::Definition::DeepFingerprint(uint64 level) const {
  return ::Fingerprint(data_.DeepFingerprint(level), ESCAPE);
}
inline uint64 Fingerprint(const Object & o, uint64 level = 0) {
  return o.DeepFingerprint(level);
}

inline Pattern TupleToPattern(const Tuple & t) {
  Pattern ret;
  for (uint i=0; i<t.size(); i++) ret.push_back(t[i]);
  return ret;
}
inline Tuple PatternToTuple(const Pattern & p) {
  Tuple ret;
  for (uint i=0; i<p.size(); i++) ret.push_back(p[i]);
  return ret;
}
inline MPattern PatternToMPattern(const Pattern &p) {
  MPattern ret;
  for (uint i=0; i<p.size(); i++) ret.push_back(p[i].Data());
  return ret;
}
inline Pattern MPatternToPattern(const MPattern &p) {
  Pattern ret;
  for (uint i=0; i<p.size(); i++) ret.push_back(OTuple::Make(p[i]));
  return ret;
}


void InitKeywords();
// keywords that need to be created in InitKeywords
extern Keyword WILDCARD;
extern OTime NEVER;

inline bool IsVariable(const Object & o) {  return (o.Type()==VARIABLE); }
inline bool IsWildcard(const Object & o) {  return (o == WILDCARD); }

void ObjectsShell();

#endif


