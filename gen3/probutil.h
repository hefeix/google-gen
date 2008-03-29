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
#include <math.h>

// random 32-bit integer
uint32 RandomUInt32();

// random 64-bit integer
uint64 RandomUInt64();

// between 0 and 1, non-inclusive
double RandomFraction();

// Randomly round off a double
int RandomRoundoff(double d);

// a particular probability distribution over unsigned ints.
// Pr(n) = 1/((n+1)(n+2))
inline double uintQuadraticProb(uint n) { return 1.0/( (n+1.0) * (n+2.0));}

// Draws from the above distribution.
uint32 RandomUintQuadratic(int max_return_value = -1);

// Normally distributed random variable N(0,1)
double RandomNormal();
double NormalDensity(double x);
double NormalDensity(double x, double mean, double std);
double LogOfNormalDensity(double x);

// Are we really using this?
// Declares A and sets it to a random element of B.
// defines an interator A pointing to a ranodm element of B.  Linear time.
#define RandomElement(A, B) typeof(B.begin()) A = B.begin(); int howfar = RandomUInt32() % B.size(); for (int count=0; count<howfar; count++) ++A;

#endif
