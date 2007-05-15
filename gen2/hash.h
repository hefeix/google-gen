/*
--------------------------------------------------------------------
lookup8.c, by Bob Jenkins, January 4 1997, Public Domain.
hash(), hash2(), hash3, and mix() are externally useful functions.
Routines to test the hash are included if SELF_TEST is defined.
You can use this free for any purpose.  It has no warranty.
--------------------------------------------------------------------
*/
#ifndef _LOOKUP8_H_
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
//
// Declarations and some additions to Bob Jenkins' public domain hash functions.


#define _LOOKUP8_H_

#include <vector>
#include <set>
#include <map>
#include <string>

using namespace std;

typedef unsigned long long uint64;   /* unsigned 8-byte quantities */
typedef long long int64;
typedef unsigned long int  uint32;   /* unsigned 4-byte quantities */
typedef unsigned char uint8;

uint64 BJHash( const uint8 *k, uint64 length, uint64 level = 0);
inline uint64 Fingerprint(const string & s, uint64 level = 0){
  return BJHash((const uint8*)s.c_str(), s.size(), level);
}
inline uint64 Fingerprint(uint64 n, uint64 level = 0){
  return BJHash((uint8*)&n, 8, level);
}
inline uint64 Fingerprint(double n, uint64 level = 0){
  return BJHash((uint8*)&n, 8, level);
}
inline uint64 Fingerprint(int64 n, uint64 level = 0){
  return BJHash((uint8*)&n, 8, level);
}
inline uint64 Fingerprint(uint32 n, uint64 level = 0){
  return BJHash((uint8*)&n, 4, level);
}
inline uint64 Fingerprint(int n, uint64 level = 0){
  return BJHash((uint8*)&n, 4, level);
}
template <class T> uint64 FingerprintIterator(const T & begin, const T & end, uint64 level = 0){
  uint64 ret = level;
  for (T run = begin; run!=end; ++run){
    ret = Fingerprint(*run, ret);
  }
  return ret;
}
template <class A, class B> uint64 Fingerprint(const pair<A,B> & p,
					       uint64 level = 0){
  return Fingerprint(p.second, Fingerprint(p.first, level));
}
template <class T> uint64 Fingerprint(const set<T> & s, uint64 level = 0){
  return FingerprintIterator(s.begin(), s.end(), level);
}
template <class A, class B> uint64 Fingerprint(const map<A,B> & s, 
					       uint64 level = 0){
  return FingerprintIterator(s.begin(), s.end(), level);
}
template <class T> uint64 Fingerprint(const vector<T> & s, uint64 level = 0){
  return FingerprintIterator(s.begin(), s.end(), level);
}

#endif 
