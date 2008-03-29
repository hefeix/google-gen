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

uint32 RandomUintQuadratic(int max_return_value) {
  while (1) {
    uint ret_val = uint (1.0 / RandomFraction()) - 1;
    if ((max_return_value != -1)
	&& (ret_val > (uint) max_return_value)) continue;
    return ret_val;
  }
}

uint32 RandomUInt32(){
  CHECK(RAND_MAX == 0x7FFFFFFF);
  return (rand() << 1) + rand()%2;
}

uint64 RandomUInt64() {
  return (uint64(RandomUInt32()) << 32) + RandomUInt32(); 
}

double TwoToTheThirtyTwo = pow(2, 32);
double TwoToTheMinusThirtyTwo = pow(0.5, 32);
double OneOverRandMax = (1.0 / RAND_MAX);
double RandomFraction() {
  return (rand() + 0.5) * OneOverRandMax;
}
int RandomRoundoff(double d) {
  int f = floor(d);
  if (d==f) return f;
  return f + ((RandomFraction() < (d-f))?1:0);
}

// Box muller transform, polar method
double RandomNormal() {
  double s = 0.0;
  double u, v;
  do {
    u = RandomFraction() * 2.0 - 1.0;
    v = RandomFraction() * 2.0 - 1.0;
    s = u*u + v*v;
  } while ( (s>=1) || (s == 0.0) );
  double z0 = u * sqrt(-2 * log(s) / s);
  return z0;
}
double ONEOVERROOT2PI = 1 / sqrt(2 * M_PI);
double LOGONEOVERROOT2PI = log(ONEOVERROOT2PI);
double NormalDensity(double x) {
  // 1 / sqrt(2PI) e^-(x^2/2)
  return ONEOVERROOT2PI * exp(-0.5*x*x);
}
double NormalDensity(double x, double mean, double std) {
  return NormalDensity((x-mean)/std) / std;
}

double LogOfNormalDensity(double x) {
  return LOGONEOVERROOT2PI - (0.5 * x * x);
}



