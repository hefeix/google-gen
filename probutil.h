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


#ifndef _PROBUTIL_H_
#define _PROBUTIL_H_

#include "util.h"
#include "numbers.h"
#include <math.h>

struct LL{
  int64 mcn_;
  double ToDouble() const { return mcn_/1e6;}
  int64 ToMicronats() const { return mcn_;}
  void operator+=(const LL & o){mcn_ += o.mcn_;}
  void operator-=(const LL & o){mcn_ -= o.mcn_;}
  string ToString() const { return dtoa(ToDouble());}
};
inline LL Micronats(int64 mcn){ LL ret; ret.mcn_=mcn; return ret;}
inline LL ToLL(double n){ return Micronats(int64(n*1e6));}
inline LL LLZero(){ return Micronats(0);}
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
LL Log(int i);

LL LnFactorial(uint n);
LL LnCombinations(uint n, uint k);

// a particular probability distribution over unsigned ints.
// Pr(n) = 1/((n+1)(n+2))
LL uintQuadraticLnProb(uint n);

// Cost of encoding a number in our funky binary representation.  
// Returns uintQuadraticLnProb(n)-n*log(2)
LL EncodedNumberLnLikelihood(const EncodedNumber & n);

// ln( 1 / ((num_total CHOOSE num_positive) (num_total+1)))
LL BinaryChoiceLnLikelihood(uint num_total, uint num_positive);
inline LL BinaryChoiceLnLikelihood(pair<int, int> p){
  return BinaryChoiceLnLikelihood(p.first, p.second);
}

#endif
