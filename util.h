// Copyright (C) 2006 Google Inc.
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
// Author: Noam Shazeer


// Functions of general utility
// macros, typedefs, etc.

#ifndef _UTIL_H_
#define _UTIL_H_

#include "hash.h"
#include <vector>
#include <string>  
#include <ext/hash_map>
#include <ext/hash_set>
#include <cstdio>
#include <iostream>
#include <cwctype>
#include <map>
#include <set>
#include <sstream>

#define OPERATORLESS operator <
#define OPERATORLE operator <=
#define OPERATOREQ operator ==
#define OPERATORGT operator >
#define OPERATORGE operator >=

#define CHECK(cond) if(!(cond)) {cerr << "Check Failed" << endl; int *x=0; *x=0; }
#define forall(A, B) for ( typeof((B).begin()) A = (B).begin(); A!=(B).end(); ++A )

// For logging
int GetVerbosity();
void SetVerbosity(int v);
#define VLOG(N) if (GetVerbosity() >=N) cerr << __FUNCTION__ << ":" << __LINE__ << " "

typedef unsigned long long uint64;
typedef unsigned long uint32;
typedef long long int64;
typedef long int32;
typedef unsigned int uint;

// magic to get hashing on strings to work 
using namespace std;
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
using namespace __gnu_cxx;

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

// read a line from a stream
bool GetLine(istream & input, string * ret);

/*
The interface to sets and maps sucks rocks. Let's fix it.

Old way: 
if (m.find(foo) != m.end()){
New way
if (m % foo){

Old way:
map<Foo, Bar>::iterator look = m.find(foo);
if (look != m.end()) {
  Bar * bar = & look->second
} else  ....

New way:
Bar * bar = m % foo;
if (bar) { ... 

*/

template<class A, class B> const B * operator %(const map<A, B> & m, 
						 const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}
template <class A, class B> const B * operator %(const hash_map<A, B> & m, 
						 const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}
template <class A, class B> B * operator %(map<A, B> & m, const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}
template <class A, class B> B * operator %(hash_map<A, B> & m, const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}
template<class A> bool operator %(const set<A> & m, const A & a){
  return m.find(a) != m.end();
}
template<class A> bool operator %(const hash_set<A> & m, const A & a){
  return m.find(a) != m.end();
}
template<class A> bool operator %(const vector<A> & v, const A & a){
  for (uint i=0; i<v.size(); i++) if (v[i]==a) return true;
  return false;
}
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

// union of two sets
template <class A> set<A> Union(const set<A> & s1, const set<A> & s2){
  set<A> ret = s1;
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
inline int atoi(const string & s){return atoi(s.c_str());}
// strips whitespace from the ends of a string
string StripWhiteEnds(const string & s);

vector<string> Split(const string & s, char delim);
string Join(const vector<string> v, char delim);
// vector of ints to a space-separated string of decimal integers
string IntVectorToString(const vector<int> v);
// inverse of above, and ignores extra whitespace
vector<int> StringToIntVector(const string & s);

// random 31-bit integer
int RandomInt();
// between 0 and 1, non-inclusive
double RandomFraction();

// Declares A and sets it to a random element of B.
// defines an interator A pointing to a ranodm element of B.  Linear time.
#define RandomElement(A, B) typeof(B.begin()) A = B.begin(); int howfar = RandomInt() % B.size(); for (int count=0; count<howfar; count++) ++A;


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

// the result of a computation that may not have finished
enum ComputationResult {
  RESULT_FALSE, // the anser is no
  RESULT_TRUE,  // the answer is yes
  RESULT_MAYBE, // I don't know, and more time won't help.
  RESULT_GAVE_UP,  // I gave up for lack of time.
};


#endif
