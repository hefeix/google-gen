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

double LnCombinations(uint n, uint k){
  CHECK(k<=n);
  return LnFactorial(n) - LnFactorial(k) - LnFactorial(n-k);
}

double LnFactorial(uint n){
  static double cache[1001];
  static bool init = false;
  if (!init) {
    init = true;
    cache[0] = 0;
    for (int i=1; i<1001; i++) cache[i] = cache[i-1] + log((double)i);    
  }
  if (n<=1000) return cache[n];
  double lower = 1000.5;
  double upper = n+0.5;
  return cache[1000] + (upper * log(upper)-upper) - (lower * log(lower)-lower);
}

double uintQuadraticLnProb(uint n){
  return -log((n+1.0)*(n+2.0));
}

double EncodedNumberLnLikelihood(const EncodedNumber & n){
  return uintQuadraticLnProb(n.bits_.size())
    - n.bits_.size() * log(2.0);
}

double BinaryChoiceLnLikelihood(uint num_total, uint num_positive){
  return -log(num_total+1) - LnCombinations(num_total, num_positive);
}


