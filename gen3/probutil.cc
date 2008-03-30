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

double StdNormalLnDensity(double x) {
  return LOGONEOVERROOT2PI - (0.5 * x * x);
}

double NormalLnDensity(double mean, double std, double x) {
  CHECK(std > 0);
  return StdNormalLnDensity((x-mean) / std) - log(std);
}

double RandomExponential(double lambda) {
  CHECK(lambda > 0.0);
  double u = RandomFraction();
  return -(log(u)) / lambda;
}

double ExponentialLnDensity(double lambda, double x) {
  // lambda e^-lambda*x
  CHECK(lambda > 0.0);
  if (x < 0) return -INF;
  return log(lambda) - lambda * x;
}

int64 RandomGeometric(double p) {
  return int64(RandomExponential(p));
}

double GeometricLnProb(double p, int64 n) {
  // (1-p)^n * p
  if (n < 0) return -INF;
  return log(p) + n * log(1-p);
}

double RandomUniform(double minimum, double maximum) {
  CHECK(maximum > minimum);
  return minimum + (RandomFraction() * (maximum - minimum));
}

double UniformLnDensity(double minimum, double maximum, double x) {
  CHECK (maximum > minimum);
  if ((x >= minimum) && (x < maximum)) {
    return - log(maximum-minimum);
  }
  return -INF;
}

int64 RandomUniformDiscrete(int64 minimum, int64 maximum) {
  CHECK(maximum > minimum);
  return RandomUInt64() % uint64(maximum - minimum) + minimum;
}

double UniformDiscreteLnProb(int64 minimum, int64 maximum, int64 x) {
  if ( (x >= minimum) && (x < maximum) )
    return -log(uint64(maximum - minimum));
  return -INF;
}





