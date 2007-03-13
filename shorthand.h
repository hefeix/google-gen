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

// This file will contain a bunch of simple and general functions, templates 
// and macros that we like to use everywhere.  No .cc is required.  

#ifndef _SHORTHAND_H_
#define _SHORTHAND_H_

#include <vector>
#include <set>
#include <map>
#include <iostream>
#include <ext/hash_map>
#include <ext/hash_set>

using namespace std;
using namespace __gnu_cxx;

#define CHECK(cond) if(!(cond)) {cerr << "Check Failed" << endl; int *x=0; *x=0; }

#define OPERATORLESS operator <
#define OPERATORLE operator <=
#define OPERATOREQ operator ==
#define OPERATORGT operator >
#define OPERATORGE operator >=

#define forall(A, B) for ( typeof((B).begin()) A = (B).begin(); A!=(B).end(); ++A )

typedef unsigned long long uint64;
typedef unsigned long uint32;
typedef long long int64;
typedef long int32;
typedef unsigned int uint;

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


#endif
