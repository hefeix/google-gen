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

LL LnFactorial(uint n){
  CHECK(n<10000000);
  static vector<LL> cache;
  while (cache.size()<=n) {
    if (cache.size()==0) cache.push_back(Micronats(0));
    cache.push_back(cache[cache.size()-1] + Log(cache.size()));
  }
  return cache[n];
  /*
    // this can be used for estimations if n gets too big.
  static bool init = false;
  if (!init) {
    init = true;
    cache[0] = 0;
    for (int i=1; i<1001; i++) cache[i] = cache[i-1] + Log(i);    
  }
  if (n<=1000) return cache[n];
  double lower = 1000.5;
  double upper = n+0.5;
  return cache[1000] + (upper * log(upper)-upper) - (lower * log(lower)-lower);
  */
}

LL uintQuadraticLnProb(uint n){
  return -Log((n+1)*(n+2));
}

LL EncodedNumberLnLikelihood(const EncodedNumber & n){
  return uintQuadraticLnProb(n.bits_.size()) - n.bits_.size() * Log(2);
}

LL BinaryChoiceLnLikelihood(uint num_total, uint num_positive){
  return -Log(num_total+1) - LnCombinations(num_total, num_positive);
}


