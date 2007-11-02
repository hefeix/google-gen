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

/*

How objects get parsed:

based on the first character:

:     It's an empty OMap
([{   It's an OTuple or an OMap.  Can be comma delimited or not. 
      if there are colons between key/value pairs, it's an OMap
      Example OTuple [ 1, 2, Georges ]
      Example OMap ( 1:1 2:4 3:Harik )
'     It's an Escape.  The escaped object is after the '
*     It's a wildcard
#     It's an OBitSeq, example #00011100
0-9.- It's an Integer or a Real.  It's a real if it has a period.
A-Z   It's a Flake. example: Noam
a-z(single character)   It's a variable.
v(followed by an integer) It's a variable. example: v26
a-z(otherwise)   It's a keyword, except for the following exceptions:
   true   the Boolean true
   false  the Boolean false
   never  the OTime NEVER
   null   a null Object.
   time (followed by an OMap)   a time.  Dimensions mapped to coordinates.
      Example: time (#00:3 #0:2 #:7 #1:12)
   pattern (followd by an OTuple of OTuples)   an OPattern
"     A string object where quotes and \ need to be escaped with \ 

*/

#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include "util.h"
#include "hash.h"
#include "numbers.h"
#include "ranktree.h"
#include "small_tree.h"
#include "allocators.h"

class Object {
 public:
#define ObjectTypeList {			\
    ITEM(OBJECT),				\
      ITEM(FLAKE),				\
      ITEM(KEYWORD),				\
      ITEM(VARIABLE),				\
      ITEM(OTUPLE),				\
      ITEM(OMAP),				\
      ITEM(OPATTERN),				\
      ITEM(BOOLEAN),				\
      ITEM(INTEGER),				\
      ITEM(REAL),				\
      ITEM(OTIME),				\
      ITEM(OBITSEQ),				\
      ITEM(STRING),				\
      ITEM(ESCAPE),				\
      ITEM(NULLTYPE),				\
      ITEM(ERRORTYPE),				\
      };
  CLASS_ENUM_DECLARE(Object, Type);
  // There will exist no two identical ObjectDefinition objects.
  struct Definition {
    virtual Object::Type GetType() const = 0;
    string ToString(bool verbose = false) const{
      string ret = ToStringSpecific(verbose);
      if (verbose) ret = "<" + Object::TypeToString(GetType()) + " rc=" 
	+ itoa(reference_count_) + "> " + ret;
      return ret;
    };
    virtual string ToStringSpecific(bool verbose) const {
      return "ERROR";
    }
    virtual ~Definition(){}
    int reference_count_;
    Definition() {
      reference_count_ = 0;
    }
    virtual uint32 DeepHash32(uint32 level = 0) const = 0;
  };
  
  Object() {
    def_ = NULL;
  }
  Object(Definition * def) {
    def_ = NULL;
    PointTo(def);
  };
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
  Type GetType() const { 
    if (def_ == NULL) return NULLTYPE;
    return def_->GetType();
  }
  // returns the type of this Object object
  virtual Type GetReferenceType() const{
    return OBJECT;
  }
  const Definition * GetDefinition() const {
    return def_;
  }
  string ToString(bool verbose=false) const 
  { if (def_) return def_->ToString(verbose); return "null"; }
  uint32 ShallowHash32(uint32 level = 0) const {
    return ::Hash32((uint32)def_, level);
  }
  uint32 DeepHash32(uint32 level = 0) const {
    return def_->DeepHash32(level);
  }

 protected:
  Definition * def_;
  void PointTo(Definition *def){
    if (def_) {
      def_->reference_count_--;
      if (def_->reference_count_==0) delete def_;
    }
    def_ = def;
    if (def_) {
      // run-time type checking.
      CHECK(GetReferenceType()==OBJECT || GetReferenceType() == def_->GetType());
      def_->reference_count_++;
    }
  }
};




inline bool operator ==(const Object & o1, const Object & o2) {
  return (o1.GetDefinition()==o2.GetDefinition());
}
inline bool operator <(const Object & o1, const Object & o2) {
  return (o1.GetDefinition()<o2.GetDefinition());
}
inline bool operator !=(const Object &o1, const Object &o2){
  return !(o1==o2);
}
inline bool operator ==(const Object & o, void *p) {
  return o.GetDefinition() == p;
}
inline bool operator !=(const Object & o, void *p) {
  return (!(o==p));
}

// TODO: we could make some types of objects that own their definitions.  
// We would have to change object comparison to first be by type.  

template <Object::Type OT, class D>  class SpecificObject : public Object {
 public:
  class Definition : public Object::Definition {
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
    Object::Type GetType() const { 
      return OT;
    }
    string ToStringSpecific(bool verbose) const;
    const D & Data() const { return data_; }
    uint32 DeepHash32(uint32 seed = 0) const{
      return ::Hash32(::Hash32(data_, uint32(OT)), seed); 
    }
  };
  static hash_map<D, Definition *> unique_;
  

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
  static SpecificObject<OT, D> Default() {
    return Make(D());
  }
  Object::Type GetReferenceType() const{
    return OT;
  }
  
  const Definition * GetDefinition() const{
    return dynamic_cast<const Definition *>(Object::GetDefinition());
  }

  const D & Data() const{ return GetDefinition()->Data();}
  //  const D & operator *() const{return Data();}
  
  // this should work for tuples.  I guess it doesn't cause compile errors
  // for the rest because it's in a template.
  Object operator [](int i) const { return Data()[i]; }
  uint size() const { return Data().size(); }

  // constructors at bottom to prevent emacs from @#($*@ing up the indentation
  static SpecificObject ConvertOrNull(const Object & o) {
    if (o.GetType() == OT)
      return SpecificObject(o);
    return NULL;
  }

  SpecificObject() : Object() {}
    
    SpecificObject(const Object & o) : Object(o) {      
      CHECK(def_ == NULL || o.GetType() == OT);    
    }
      
      SpecificObject(Object::Definition *def) 
	: Object(def) {
	CHECK(def == NULL || def->GetType() == OT);
      }
	
	//SpecificObject(void *null) : Object(null) {}
};


namespace __gnu_cxx{
  template <Object::Type OT, class D> 
    class hash<SpecificObject<OT, D> > {
  public:
    size_t operator()(const SpecificObject<OT, D> & o) const{
      return size_t(o.GetDefinition());
    }
  };
};

istream & operator >>(istream & input, Object & o);
inline Object StringToObject(string s) {
  istringstream istr(s);
  Object o;
  istr >> o;
  return o;
}

inline ostream & operator <<(ostream & output, const Object & o) {
  output << o.ToString();
  return output;
}

namespace __gnu_cxx{
  template <> class hash<Object> {
  public:
    size_t operator()(const Object & o) const{
      return size_t(o.GetDefinition());
    }
  };
};

// map<int, int, less<int>, MyAlloc<std::pair<int const, int > > > test_vector;

typedef vector<Object> Tuple;
typedef map<Object, Object> Map;
//typedef map<Object, Object, less<Object>, MyAlloc<pair<Object const, Object> > > Map;
typedef vector<Tuple> MPattern; // mutable 2 levels down

typedef SpecificObject<Object::FLAKE, string> Flake;
typedef SpecificObject<Object::KEYWORD, string> Keyword;
typedef SpecificObject<Object::VARIABLE, int> Variable;
typedef SpecificObject<Object::OTUPLE, Tuple > OTuple;
typedef SpecificObject<Object::OMAP, Map> OMap;
typedef SpecificObject<Object::BOOLEAN, bool> Boolean;
typedef SpecificObject<Object::INTEGER, int> Integer;
typedef SpecificObject<Object::REAL, double> Real;
typedef SpecificObject<Object::OTIME, Time> OTime;
typedef vector<OTuple> Pattern;
typedef SpecificObject<Object::OPATTERN, Pattern> OPattern;
typedef SpecificObject<Object::OBITSEQ, BitSeq> OBitSeq;
typedef SpecificObject<Object::STRING, string> String;
typedef SpecificObject<Object::ESCAPE, Object> Escape;

typedef set<Variable> VariableSet;


inline const Object * operator %(const OMap & m, Object key) {
  return m.Data() % key;
}

template <> 
inline uint32 OTuple::Definition::DeepHash32(uint32 level) const {
  uint32 ret = ::Hash32((uint32)OTUPLE, level);
  forall(run, data_) ret = run->DeepHash32(ret);
  return ret;
}
template <> 
inline uint32 OMap::Definition::DeepHash32(uint32 level) const {
  uint32 ret = ::Hash32((uint32)OMAP, level);
  forall(run, data_) {
    ret = run->first.DeepHash32(ret);
    ret = run->second.DeepHash32(ret);
  }
  return ret;
}
template <> 
inline uint32 Escape::Definition::DeepHash32(uint32 level) const {
  return ::Hash32(data_.DeepHash32(level), ESCAPE);
}
inline uint32 Hash32(const Object & o, uint32 level = 0) {
  return o.DeepHash32(level);
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

template <class T> struct DataCompare{
  bool operator()(const T& t1, const T& t2) {
    if (t2 == NULL) return false;
    if (t1 == NULL) return true;
    return (t1.Data() < t2.Data());
  }
};

template <class T> const T & DataMax(const T & t1, const T & t2) {
  if (DataCompare<T>()(t2, t1)) return t1;
  return t2;
}
template <class T> const T & DataMin(const T & t1, const T & t2) {
  if (DataCompare<T>()(t1, t2)) return t1;
  return t2;
}


void InitConstants();
void DestroyConstants();
// keywords that need to be created in InitKeywords
extern Keyword WILDCARD;
extern Keyword SEMICOLON;
extern OTime CREATION;
extern OTime NEVER;
extern Boolean TRUE;
extern Boolean FALSE;

inline bool IsVariable(const Object & o) { 
  return (o.GetType()==Object::VARIABLE); 
}
inline bool IsWildcard(const Object & o) {  
  return (o == WILDCARD); 
}

void ObjectsShell();

#endif


