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


#ifndef _PROBUTIL_H_
#define _PROBUTIL_H_

#include "util.h"
#include "numbers.h"
#include <math.h>

struct LL{
  int64 mcn_;
  LL() { mcn_ = 0; }
  explicit LL(double d){ mcn_ = int64(d*1e6);}
  LL(int n){ mcn_ = n * 1000000ll;}
  double ToDouble() const { return mcn_/1e6;}
  int64 ToMicronats() const { return mcn_;}
  void operator+=(const LL & o){mcn_ += o.mcn_;}
  void operator-=(const LL & o){mcn_ -= o.mcn_;}
  string ToString() const { return dtoa(ToDouble());}
};
inline LL Micronats(int64 mcn){ LL ret; ret.mcn_=mcn; return ret;}
LL operator +(const LL & a, const LL & b);
LL operator -(const LL & a, const LL & b);
LL operator *(const LL & a, int b);
LL operator *(int b, const LL & a);
LL operator -(const LL & a);
bool OPERATORLESS(const LL & a, const LL & b);
bool OPERATORLE(const LL & a, const LL & b);
bool OPERATOREQ(const LL & a, const LL & b);
bool OPERATORGT(const LL & a, const LL & b);
bool OPERATORGE(const LL & a, const LL & b);
bool operator !=(const LL & a, const LL & b);

ostream & operator << (ostream & output, LL ll);
inline LL Log(double d) {
  CHECK(d>0);
  return Micronats((int)(log(d)*1000000));
}

LL LnFactorial(uint n);
LL LnCombinations(uint n, uint k);

// a particular probability distribution over unsigned ints.
// Pr(n) = 1/((n+1)(n+2))
LL uintQuadraticLnProb(uint n);

// Draws from the above distribution.
uint32 RandomUintQuadratic(int max_return_value = -1);

// Cost of encoding a number in our funky binary representation.  
// Returns uintQuadraticLnProb(length)-length*log(2)
LL BitSeqLnLikelihood(const BitSeq & n);

// ln( 1 / ((num_total CHOOSE num_positive) (num_total+1)))
LL BinaryChoiceLnLikelihood(uint num_total, uint num_positive);
inline LL BinaryChoiceLnLikelihood(pair<int, int> p){
  return BinaryChoiceLnLikelihood(p.first, p.second);
}

LL DoubleLnLikelihood(double d);
LL StringLnLikelihood(string s);
double RandomDouble();

#endif
