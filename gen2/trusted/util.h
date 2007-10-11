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


// Functions of general utility
// macros, typedefs, etc.

#ifndef _UTIL_H_
#define _UTIL_H_

#include <sys/time.h>
#include "hash.h"
#include "shorthand.h"
#include <string>  
#include <cstdio>
#include <cwctype>
#include <sstream>

/*
  This stuff helps you more simply declare enums which convert to and 
  from string values. The enums are class members. 

  for example, if we have a class called Object and want to define an enum
  Object::Type, in the .h we put 
  
  class Object {
    #define ObjectTypeList {				\
      ITEM(A),						\
      ITEM(B),						\
      ITEM(C),						\
      ITEM(D),						\
      ITEM(E),					\
      };
    CLASS_ENUM_DECLARE(Object, Type);
  };
 
  and in the .cc 
  #undef ITEM
  #define ITEM(x) #x
  CLASS_ENUM_DEFINE(Object, Type);

  This declares an enum Object::Type and declares and defines the functions
  string Object::TypeToString(Object::Type);
  Object::Type Object::StringToType(string);
  int Object::NumObjectTypes();
*/

#define CLASS_ENUM_DECLARE(CLASS, ENUM)  \
  enum ENUM CLASS##ENUM##List	\
  static char * ENUM##Names[];		     \
  static int Num##ENUM##s();			    \
  static string ENUM##ToString(ENUM e);             \
  static ENUM StringTo##ENUM(string s);		    
  

#define CLASS_ENUM_DEFINE(CLASS, ENUM)			\
  char * CLASS::ENUM##Names[] = CLASS##ENUM##List	\
  int CLASS::Num##ENUM##s() {  \
    return sizeof(CLASS::ENUM##Names) / sizeof(char *); }	\
    string CLASS::ENUM##ToString(ENUM e) {			\
      return ENUM##Names[e];}						\
  CLASS::ENUM CLASS::StringTo##ENUM(string s) {				\
    for (int i=0; i<Num##ENUM##s(); i++)				\
      if (ENUM##Names[i]==s) return ENUM(i);				\
    return ENUM(-1); } 

#define ITEM(x) x


// For logging
int GetVerbosity(string function);
void SetVerbosity(int v);
void SetVerbosity(string function_name, int v);

#define VERBOSITY (GetVerbosity(__FUNCTION__))
#define VLOG(N) if (VERBOSITY >=N) cerr << __FUNCTION__ << ":" << __LINE__ << " "

#define MOREWORK(x) if (max_work_now) {(*max_work_now)-=(x); \
    if (*max_work_now<0) return false;}

#define RETURN_TRACK(x) { typeof(x) y = x; AddReturnValue(__FUNCTION__, __LINE__, ReturnValueToString(y)); return y; }

template <class T>
string ReturnValueToString(T t) {
  stringstream s;
  s << t;
  return s.str();
}

template<>
string ReturnValueToString(bool t);

void AddReturnValue(string func, int line, string val);
string FunctionReturnInfo(string func, bool verbose);
string AllFunctionReturnInfo(bool verbose);

// A simple timing class
class Timer {
 public:
  Timer(string mesg, uint64 * acc) {
    mesg_ = mesg;
    acc_ = acc;
    int ret = gettimeofday(&tv_, NULL);
    CHECK(ret == 0);
  }

  ~Timer() {
    struct timeval now;
    int ret = gettimeofday(&now, NULL);
    CHECK(ret == 0);
    uint64 sdiff = now.tv_sec - tv_.tv_sec;
    uint64 mdiff = now.tv_usec - tv_.tv_usec;
    uint64 diff = sdiff * 1000000 + mdiff;
    if (acc_) *acc_ += diff;
    cerr << mesg_ << " T:" << diff << endl;
  }
  
 private:
  struct timeval tv_;
  string  mesg_;
  uint64 *acc_;
};

using namespace std;
// magic to get hashing on strings to work 
namespace __gnu_cxx{
  template <> class hash<string> {
  public:
    size_t operator()(const string & s) const{
      return h(s.c_str());
    }
  private:
    hash<const char *> h;
  };
  template <> class hash<uint64> {
  public:
    size_t operator()(const uint64 & n) const{
      return n;
    }
  };
};

// converting from numbers to their ascii representations.
inline string itoa(int n) {
  char p[15];
  sprintf(p, "%d", n);
  return p;
}
inline string dtoa(double d) {
  char p[20];
  sprintf(p, "%g", d);
  return p;
}
inline string ptoa(const void *ptr) {
  ostringstream output;
  output << ptr;
  return output.str();
}
inline void * atop(const string & s) {
  istringstream input(s);
  void *ret;
  input >> ret;
  return ret;
}

// read a line from a stream
bool GetLine(istream & input, string * ret);

// remove an item from a vector
template<class A> vector<A> RemoveFromVector(const vector<A> & v, uint index){
  vector<A> ret;
  for (uint i=0; i<v.size(); i++) if (i!=index) ret.push_back(v[i]);
  return ret;
}
// concatenate two vectors
template<class A> vector<A> Concat(const vector<A> & v, const vector<A> & w){
  vector<A> ret = v;
  ret.insert(ret.end(), w.begin(), w.end());
  return ret;
}

// union of two sets or maps
template <class A> A Union(const A & s1, const A & s2){
  A ret = s1;
  ret.insert(s2.begin(), s2.end());
  return ret;
}
// intersection of two sets
template <class A> set<A> Intersection(const set<A> & s1, const set<A> & s2){
  if (s2.size() < s1.size()) return Intersection(s2, s1);
  set<A> ret;
  forall(run, s1) {
    if (s2 % (*run)) ret.insert(*run);
  }
  return ret;
}

// difference of two sets
template <class A> set<A> operator-(const set<A> & s1, const set<A> & s2){
  set<A> ret = s1;
  forall(run, s2) ret.erase(*run);
  return ret;
}
// extracts keys of a map and puts them in a vector
template<class A, class B> vector<A> VectorOfKeys(const map<A, B> &m){
  vector<A> ret;
  for (typeof(m.begin()) run = m.begin(); run!=m.end(); run++){
    ret.push_back(run->first);
  }
  return ret;
}
// same for values
template<class A, class B> vector<B> VectorOfValues(const map<A, B> &m){
  vector<B> ret;
  for (typeof(m.begin()) run = m.begin(); run!=m.end(); run++){
    ret.push_back(run->second);
  }
  return ret;
}
// given a map of items to counts, adds to a count, and removes the item
// if the new count is zero.
template<class A, class B> void SparseAdd(map<A,B> * m, const A & a, 
					  const B & b) {
  B & val_ref = (*m)[a];
  val_ref += b;
  if (val_ref==0) m->erase(a);
}

template <class C> int CountRange(C start, C end){
  int ret = 0;
  for (C run=start; run!=end; run++){
    ret++;
  }
  return ret;
}

template<class A, class B> pair<A,B> operator +(const pair<A,B>& p1,
						const pair<A,B>& p2){
  return make_pair(p1.first+p2.first, p1.second+p2.second);
}
template<class A, class B> pair<A,B> operator -(const pair<A,B>& p1,
						const pair<A,B>& p2){
  return make_pair(p1.first-p2.first, p1.second-p2.second);
}

template<class A, class B> set<A> Domain(const map<A, B> & m) {
  set<A> ret;
  forall(run, m) ret.insert(run->first);
  return ret;
}

inline int atoi(const string & s){return atoi(s.c_str());}
// strips whitespace from the ends of a string
string StripWhiteEnds(const string & s);

vector<string> Split(const string & s, char delim);
string Join(const vector<string> v, char delim);
// vector of ints to a space-separated string of decimal integers
string IntVectorToString(const vector<int> v);
// inverse of above, and ignores extra whitespace
vector<int> StringToIntVector(const string & s);

// random 32-bit integer
uint32 RandomUInt32();
// between 0 and 1, non-inclusive
double RandomFraction();

// Declares A and sets it to a random element of B.
// defines an interator A pointing to a ranodm element of B.  Linear time.
#define RandomElement(A, B) typeof(B.begin()) A = B.begin(); int howfar = RandomUInt32() % B.size(); for (int count=0; count<howfar; count++) ++A;

int RandomRoundoff(double d);

// Extract a sample from a rankset or rankmap.
template <class S>
void GetSample(const S & full, S * result, uint sample_size) {
  CHECK(sample_size < full.size());
  result->clear();
  if (sample_size * 3 >= full.size()) {
    int to_pick = sample_size;
    int out_of = full.size();
    forall(run, full) {
      if (RandomFraction() * out_of < to_pick) {
	result->insert(*run);
	to_pick--;
      }
      out_of--;
    }
    return;
  }
  set<int> picked;
  for (uint i=0; i<sample_size; i++) {
    int n;
    do { 
      n = RandomUInt32() % full.size();
    } while (!(picked % n));
    picked.insert(n);
    result->insert(*(full.nth(n)));
  }  
}

#define EMPTY_SLOT (-1)
// runs through distinct assignments of a number of distincet items to a 
// greater or equal number of slots (which can fit at most one item each). 
// The items are represented by the first non-negative integers, and the
// empty slots by EMPTY_SLOT.
class PermutationIterator {
 public:
  PermutationIterator(int num_items, int num_slots);
  void operator ++();
  inline bool done() const { return done_;}
  inline const vector<int> & current() const {return slot_to_item_;}
 private:
  int num_slots_;
  int num_items_;
  vector<int> slot_to_item_; // the thing we expose
  vector<int> item_to_slot_;
  bool done_;
  void Move(int item, int new_slot);
};

// runs through all vector of non-negative integers such that each element
// in the vector is less than the corresponding value of a bounding vector
// We are using this for computing cross-products of vectors of vectors.
// Pass in the sizes of the vectors in bounds, and the iterator iterates through
// vectors of indices.
class ProductIterator {
 public:
  ProductIterator(vector<uint> bounds);
  void operator ++();
  inline bool done() const { return done_;}
  inline const vector<uint> & Current() const {return current_;}
 private:
  vector<uint> bounds_;
  vector<uint> current_;
  bool done_;
};



// the result of a computation that may not have finished
enum ComputationResult {
  RESULT_FALSE, // the anser is no
  RESULT_TRUE,  // the answer is yes
  RESULT_MAYBE, // I don't know, and more time won't help.
  RESULT_GAVE_UP,  // I gave up for lack of time.
};

// Given a set of num_objects objects numbered from 0..num_objects-1 and a 
// symmetric adjacency matrix (self-adjacency allowed and ignored), we 
// find the connected components and number them from 
// 0..num_connected_components-1.  We return the number of connected components
// and pass back in *components a vector mapping object to component.
int ConnectedComponents(int num_objects, 
			const map<int, set<int> > & adjancency_matrix,
			vector<int> * components);

// We don't want to use C++'s constructors, since they don't know about 
// subclasses, so we use L1_Init() to construct class hierarchies.  New is a 
// factory method that constructs objects using new and calls L1_Init() on them.
template<class T> T * New() {
  T *t = new T;
  t->L1_Init();
  return t;
}
template<class T, class P1> T * New(const P1 & p1) {
  T *t = new T;
  t->L1_Init(p1);
  return t;
}
template<class T, class P1, class P2> T * New(const P1 & p1, const P2 & p2) {
  T *t = new T;
  t->L1_Init(p1, p2);
  return t;
}
template<class T, class P1, class P2, class P3> T * New(const P1 & p1, 
							const P2 & p2,
							const P3 & p3) {
  T *t = new T;
  t->L1_Init(p1, p2, p3);
  return t;
}

/*template<class T> set<T> SingletonSet(const T & t) {
  set<T> ret;
  ret.insert(t);
  return ret;
  }*/

// In general SK and MK are identical, but we should at least be able to
// cast an MK to an SK
template<class S, class M> M Restrict(const M & m, 
				      const S & s) {
  M ret;
  forall(run, m) {
    if (s % (typeof(*s.begin()))run->first) ret[run->first] = run->second;
  }
  return ret;
}

/* Usage of the New paradigm that calls Init
   Statement * st = New<Statement>(param1, param2, param3)
*/

string Downcase(string s);
string Upcase(string s);

template <class S> S Singleton(typename S::key_type t) {
  S ret; ret.insert(t); return ret;
}
/*template<class T> set<T> SingletonSet(const T & t) {
  set<T> ret; ret.insert(t);
  return ret;
  }*/

#endif
