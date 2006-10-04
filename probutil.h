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

double LnFactorial(uint n);
double LnCombinations(uint n, uint k);

// a particular probability distribution over unsigned ints.
// Pr(n) = 1/((n+1)(n+2))
double uintQuadraticLnProb(uint n);

// Cost of encoding a number in our funky binary representation.  
// Returns uintQuadraticLnProb(n)-n*log(2)
double EncodedNumberLnLikelihood(const EncodedNumber & n);

// picks the strength of a rule to maximize the probability of the
// observations multiplied by the probability of the strength according to the
// above function.
EncodedNumber PickOptimalRuleStrength(uint total_instances, 
				      uint positive_instances);
#endif
