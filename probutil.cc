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

const double LN2 = log(2);

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
    for (int i=0; i<1001; i++) cache[i] = cache[i-1] + log(i);    
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
    - n.bits_.size() * log(2);
}

EncodedNumber PickOptimalRuleStrength(uint total_instances, 
				      uint positive_instances){
  double negative_instances = total_instances - positive_instances;
  if (total_instances == 0) return EncodedNumber();
  int max_bits = 1 >? (int)(2 * log (total_instances)); // TODO: Check this
  double p_ideal = (double(positive_instances)) / total_instances;
  EncodedNumber current;
  EncodedNumber best;
  double max_combined_lob_prob = 0.0;
  for (int i=0; i<max_bits; i++) {
    double p = current.ToOpenInterval();
    double combined_log_prob = EncodedNumberLnLikelihood(current)
      + positive_instances*log(p) + negative_instances * log(1-p)
      - i * LN2;
    if (i==0 || combined_log_prob > max_combined_lob_prob) {
      best = current;
      max_combined_lob_prob = combined_log_prob;
    }
    current.bits_.push_back(p_ideal > p);
  }
  return best;
}

