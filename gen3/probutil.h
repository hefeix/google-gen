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

// Normally distributed random variable N(0,1)
double RandomNormal();
double NormalLnDensity(double mean, double std, double x);

// Exponential distribution
double RandomExponential(double lambda);
double ExponentialLnDensity(double lambda, double x);

// Geometric distribution
int64 RandomGeometric(double p);
double GeometricLnProb(double p, int64 n);

// Uniform distribution
double RandomUniform(double minimum, double maximum);
double UniformLnDensity(double minimum, double maximum, double x);

// Uniform discrete distribution [minimum, maximum)
int64 RandomUniformDiscrete(int64 minimum, int64 maximum);
double UniformDiscreteLnProb(int64 minimum, int64 maximum, int64 x);

// Are we really using this?
// Declares A and sets it to a random element of B.
// defines an interator A pointing to a ranodm element of B.  Linear time.
#define RandomElement(A, B) typeof(B.begin()) A = B.begin(); int howfar = RandomUInt32() % B.size(); for (int count=0; count<howfar; count++) ++A;

#endif
