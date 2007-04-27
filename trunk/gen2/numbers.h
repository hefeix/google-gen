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


#ifndef _NUMBERS_H_
#define _NUMBERS_H_

#include "util.h"

// represents a real number between zero and 1 by a vector of bits, most 
// significant bit first.
// If there are k bits which in binary represent the number n, then the number
// represented is (n+1/2)*2^-k.
struct EncodedNumber {
  vector<bool> bits_;
  EncodedNumber() {};
  EncodedNumber(const string & s) {  FromSortableString(s); }
  string ToSortableString() const;
  void FromSortableString(const char ** p);
  void FromSortableString(const string & s) {
    const char * p = s.c_str(); 
    FromSortableString(&p);
  }
};
inline istream & operator >> (istream & input, EncodedNumber & n){
  string s;
  input >> s;
  n.FromSortableString(s);
  return input;
}
inline ostream & operator << (ostream & output, const EncodedNumber &n){
  output << n.ToSortableString();
  return output;
}

bool OPERATORLESS(const EncodedNumber & a, const EncodedNumber & b);
bool OPERATOREQ(const EncodedNumber & a, const EncodedNumber & b);
inline bool OPERATORLE(const EncodedNumber & a, const EncodedNumber & b) {
  return (!(b<a));
}
inline bool operator !=(const EncodedNumber & a, const EncodedNumber & b){return !(a==b);}

string IntToSortableString(int x); // only on positive integers
int IntFromSortableString(const char ** p);
inline int IntFromSortableString(const string & s){
  const char * p = s.c_str(); 
  return IntFromSortableString(&p);
}

// A time is a vector in infinite-dimensional space.  The dimensions are labeled
// by EncodedNumbers, and the coordinates in those dimensions are non-negative
// integers.  Only a finite number of these coordinates are non-zero.
// Dimensions labeled by larger EncodedNumbers are more significant.  
//
// A time can be incremented by a particular number of steps in a given
// dimension.  The coordinate in that dimension is increased by that count, 
// and all less significant dimensions are removed.  
//
// Times can be represented by strings in an order-preserving manner by listing
// coordinates (with labels), most significant first.
//
// The artificial time NEVER (like inf) is  greater than every other time.  
// The time CREATION has a zero in every coordinate and is less than every
// other time.
struct Time{
  bool never_; // if true, this time is NEVER
  vector<pair<EncodedNumber, int> > coordinates_;

  Time() {never_ = false;} // times default to CREATION
  Time(const string & s) { FromSortableString(s);}
  string ToSortableString() const;
  void FromSortableString(const char ** p);
  void FromSortableString(const string & s) {
    const char * p = s.c_str(); 
    FromSortableString(&p);
  }
  void Increment(const EncodedNumber & coordinate, int count);
  bool IsNever() { return never_;}
  static Time Never() { Time t; t.never_ = true; return t;}
};
inline Time operator +(const Time & t, EncodedNumber coordninate){
  Time ret = t;
  ret.Increment(coordninate, 1);
  return ret;
}

inline istream & operator >> (istream & input, Time & n){
  string s;
  input >> s;
  n.FromSortableString(s);
  return input;
}
inline ostream & operator << (ostream & output, const Time &n){
  output << n.ToSortableString();
  return output;
}

#define NEVER Time::Never()
#define CREATION Time()
bool OPERATORLESS(const Time & a, const Time & b);
bool OPERATOREQ(const Time & a, const Time & b);
inline bool OPERATORGT(const Time & a, const Time & b) { return (b<a);}
inline bool OPERATORLE(const Time & a, const Time & b) { return !(b<a);}
inline bool OPERATORGE(const Time & a, const Time & b) { return !(a<b);}
inline bool operator !=(const Time & a, const Time & b){return !(a==b);}
void TestNumbersShell(); // shell for testing this module

#endif
