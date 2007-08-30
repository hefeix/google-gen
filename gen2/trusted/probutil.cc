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


#include "probutil.h"

const double LN2 = log(2.0);

LL operator +(const LL & a, const LL & b) { return Micronats(a.mcn_+b.mcn_); }
LL operator -(const LL & a, const LL & b) { return Micronats(a.mcn_-b.mcn_); }
LL operator *(const LL & a, int b) { return Micronats(a.mcn_*b); }
LL operator *(int b, const LL & a) { return a*b;}
LL operator -(const LL & a){ return Micronats(-a.mcn_);} 
ostream & operator << (ostream & output, LL ll){
  return (output << ll.ToString());
}
LL Log(int i){
  CHECK(i>0);
  return Micronats((int)(log(i)*1000000));
}
bool OPERATORLESS(const LL & a, const LL & b){ return (a.mcn_<b.mcn_);}
bool OPERATORLE(const LL & a, const LL & b){ return (a.mcn_<=b.mcn_);}
bool OPERATOREQ(const LL & a, const LL & b){ return (a.mcn_==b.mcn_);}
bool OPERATORGT(const LL & a, const LL & b){ return (a.mcn_>b.mcn_);}
bool OPERATORGE(const LL & a, const LL & b){ return (a.mcn_>=b.mcn_);}
bool operator !=(const LL & a, const LL & b){ return (a.mcn_!=b.mcn_);}



LL LnCombinations(uint n, uint k){
  CHECK(k<=n);
  return LnFactorial(n) - LnFactorial(k) - LnFactorial(n-k);
}

// exact product of Log() if n<max_factorial_cache_size.  Otherwise an estimate.

const uint max_factorial_cache_size = 10000;
LL LnFactorial(uint n){
  static vector<LL> cache;
  while (cache.size()<=(min(n, max_factorial_cache_size)) ) {
    if (cache.size()==0) cache.push_back(Micronats(0));
    cache.push_back(cache[cache.size()-1] + Log(cache.size()));
  }
  if (n < max_factorial_cache_size)
    return cache[n];
  double lower = max_factorial_cache_size-0.5;
  double upper = n+0.5;
  return LL( cache[max_factorial_cache_size-1].ToDouble() 
	     + (upper * log(upper) - upper )
	     - (lower * log(lower) - lower )
	     );
}

LL uintQuadraticLnProb(uint n){
  return -Log((n+1)*(n+2));
}

LL BitSeqLnLikelihood(const BitSeq & n){
  int nb = n.NumBits();
  return uintQuadraticLnProb(nb) - nb * Log(2);
}

LL BinaryChoiceLnLikelihood(uint num_total, uint num_positive){
  return -Log(num_total+1) - LnCombinations(num_total, num_positive);
}


