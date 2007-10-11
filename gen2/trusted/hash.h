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
// Author: Noam Shazeer and Georges Harik
//
// Declarations and some additions to Bob Jenkins' public domain hash functions.

#ifndef _HASH_H_
#define _HASH_H_

#include <vector>
#include <set>
#include <map>
#include <string>
#include <ext/hash_map>
#include <ext/hash_set>

#include "shorthand.h"

inline uint32 Hash32(uint32 a, uint32 seed = 0) {
  return a * 30534821 + seed * 54359087 + 42342389;
}
inline uint32 Hash32(const uint8 * s, uint length, uint32 seed = 0) {
  uint32 ret = 98439481;
  uint i;
  for (i=0; i*sizeof(uint32) < length; i++) {
    uint32 x;
    memcpy(&x, s + i * sizeof(uint32), sizeof(uint32));
    ret = Hash32(ret, x);
  }
  if (length % sizeof(uint32)) {
    uint32 x = 0;
    memcpy(&x, s + i * sizeof(uint32), length % sizeof(uint32));
    ret = Hash32(ret, x);
  }
  return ret;
}
inline uint32 Hash32(const uint32 * s, int num_int32s, uint32 seed = 0) {
  uint32 ret = 24357213;
  for (int i=0; i<num_int32s; i++) ret = Hash32(ret, s[i]);
  return ret;
}

inline uint32 Hash32(const string & s, uint32 seed = 0){
  return Hash32((const uint8*)s.c_str(), s.size(), seed);
}
inline uint32 Hash32(uint64 n, uint32 seed = 0){
  return Hash32((uint32*)&n, 2, seed);
}
inline uint32 Hash32(double n, uint32 seed = 0){
  return Hash32((uint32*)&n, 2, seed);
}
inline uint32 Hash32(int64 n, uint32 seed = 0){
  return Hash32((uint32*)&n, 2, seed);
}
inline uint32 Hash32(int32 n, uint32 seed = 0){
  return Hash32(uint32(n), seed);
}
inline uint32 Hash32(bool n, uint32 seed = 0){
  return Hash32(uint32(n), seed);
}
inline uint32 Hash32(int n, uint32 seed = 0){
  return Hash32(uint32(n), seed);
}

template <class T> uint32 Hash32Iterator(const T & begin, 
					 const T & end, 
					 uint32 level = 0){
  uint32 ret = level;
  for (T run = begin; run!=end; ++run){
    ret = Hash32(*run, ret);
  }
  return ret;
}
// TODO: this is bad, since sets of integers with the sae sum
// hash to the same value.   Fix this.  
template <class T> uint32 Hash32IteratorUnordered(const T & begin, 
						  const T & end, 
						  uint32 level = 0){
  uint32 ret = 23493821;
  for (T run = begin; run!=end; ++run){
    ret += Hash32(*run, 52385711);
  }
  return ret;
}

template <class A, class B> uint32 Hash32(const pair<A,B> & p,
					  uint32 level = 0){
  return Hash32(p.second, Hash32(p.first, level));
}
template <class T> uint32 Hash32(const set<T> & s, uint32 level = 0){
  return Hash32Iterator(s.begin(), s.end(), level);
}
template <class A, class B> uint32 Hash32(const map<A,B> & s, 
					  uint32 level = 0){
  return Hash32Iterator(s.begin(), s.end(), level);
}
template <class T> uint32 Hash32(const vector<T> & s, uint32 level = 0){
  return Hash32Iterator(s.begin(), s.end(), level);
}
template <class A, class B> uint32 Hash32(const hash_map<A,B> & s, 
					  uint32 level = 0){
  return Hash32IteratorUnordered(s.begin(), s.end(), level);
}

#define DEFINE_HASH_CLASS(Classname)	\
  class hash<Classname> {			\
  public:					\
    size_t operator()(const Classname & c) const{	\
      return size_t(Hash32(c));			\
    } \
  };
#define __COMMA ,

#define DEFINE_HASH_CLASS_0(Classname)				\
  namespace __gnu_cxx{ template <> DEFINE_HASH_CLASS(Classname); };
#define DEFINE_HASH_CLASS_1(Classname) \
  namespace __gnu_cxx{ template <class A>		\
      DEFINE_HASH_CLASS(Classname<A>); };
#define DEFINE_HASH_CLASS_2(Classname)		   \
  namespace __gnu_cxx{ template <class A __COMMA class B>	\
      DEFINE_HASH_CLASS(Classname<A __COMMA B>); };

DEFINE_HASH_CLASS_0(bool);
DEFINE_HASH_CLASS_0(double);
DEFINE_HASH_CLASS_1(vector);
DEFINE_HASH_CLASS_1(set);
DEFINE_HASH_CLASS_2(map);
DEFINE_HASH_CLASS_2(hash_map);

#endif 
