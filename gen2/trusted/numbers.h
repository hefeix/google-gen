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
#include "allocators.h"

inline int NumOnes(uint32 x){
  x = ( (x & 0xAAAAAAAA) >> 1) + (x & 0x55555555);
  x = ( (x & 0xCCCCCCCC) >> 2) + (x & 0x33333333);
  x = ( (x & 0xF0F0F0F0) >> 4) + (x & 0x0F0F0F0F);
  x = ( (x & 0xFF00FF00) >> 8) + (x & 0x00FF00FF);
  x = ( (x & 0xFFFF0000) >> 16) + (x & 0x0000FFFF);  
  return x;
}
inline int NumTrailingZeros(uint32 x) {
  if (x==0) return 32;
  return NumOnes(x ^ (x-1)) - 1;
}
inline int NumOnes(uint64 x){
  x = ( (x & 0xAAAAAAAAAAAAAAAAull) >> 1)  + (x & 0x5555555555555555ull);
  x = ( (x & 0xCCCCCCCCCCCCCCCCull) >> 2)  + (x & 0x3333333333333333ull);
  x = ( (x & 0xF0F0F0F0F0F0F0F0ull) >> 4)  + (x & 0x0F0F0F0F0F0F0F0Full);
  x = ( (x & 0xFF00FF00FF00FF00ull) >> 8)  + (x & 0x00FF00FF00FF00FFull);
  x = ( (x & 0xFFFF0000FFFF0000ull) >> 16) + (x & 0x0000FFFF0000FFFFull);  
  x = ( (x & 0xFFFFFFFF00000000ull) >> 32) + (x & 0x00000000FFFFFFFFull);  
  return x;
}
inline int NumTrailingZeros(uint64 x) {
  if (x==0) return 64;
  return NumOnes(x ^ (x-1)) - 1;
}

struct DoubleBits{
  uint64 mantissa_:52;
  uint32 exponent_:11;
  uint32 sign_:1;

  DoubleBits(double x) {
    memcpy(this, &x, sizeof(double));
  };
  DoubleBits(uint64 m, int32 e, int s)
    :mantissa_(m), exponent_(e+1023), sign_(s) {}
  double ToDouble(){
    double ret;
    memcpy(&ret, this, sizeof(double));
    return ret;
  }

  string ToString() const {
    ostringstream ostr;
    ostr << (IsNegative()?'-':'+') << "(1+(" << MantissaBits() 
	 << "/2^" << NumMantissaBits() << ")) * 2^" << Exponent();
    return ostr.str();
  }

  bool IsNegative() const {return sign_;}
  int Exponent() const {return exponent_-1023;}
  uint64 Mantissa() const { return mantissa_;}

  int NumMantissaBits() const { 
    return 52 - NumTrailingZeros(mantissa_ + (1ll << 52) );
  }
  uint64 MantissaBits() const {
    return Mantissa() >> (52-NumMantissaBits());    
  }
};



/*
  A bit sequence is a finite sequence of bits with an implied ordering. 
  appending a 0 makes a sequence smaller and a 1 makes it larger. 

  We store a bit sequence as a vector of booleans terminated by an extra true.
  This way, they compare correctly by the default lexicographic comparison of 
  vectors. 
*/

struct BitSeq{
  BitSeq() { data_.push_back(true);}

  vector<bool> data_;

  int NumBits() const {
    return data_.size() - 1;
  }
  bool operator [](int i) const{
    return (data_[i]);
  }
  static BitSeq Min() {
    BitSeq s;
    s.data_.clear();
    return s;
  }
  bool IsMin() const {
    return data_.size() == 0;
  }
  void AppendInPlace(bool bit) {
    CHECK(!IsMin());
    data_[NumBits()] = bit;
    data_.push_back(true);
  }
  void PopBackInPlace() {
    CHECK(NumBits() > 0);
    data_.pop_back();
    data_[NumBits()] = true;
  }
  string ToString() const{
    if (IsMin()) return "#m";
    string ret = "#";
    for (int i=0; i<NumBits(); i++) {
      ret += (*this)[i]?'1':'0';
    }
    return ret;
  }
};
inline bool operator <(const BitSeq & s1, const BitSeq & s2) {
  return s1.data_ < s2.data_;
}
inline bool operator ==(const BitSeq & s1, const BitSeq & s2) {
  return s1.data_ == s2.data_;
}
inline bool operator !=(const BitSeq & s1, const BitSeq & s2) {
  return s1.data_ != s2.data_;
}
inline bool operator >(const BitSeq & s1, const BitSeq & s2) {
  return s1.data_ > s2.data_;
}
inline BitSeq Append(const BitSeq & s, bool bit) {
  BitSeq ret = s;
  ret.AppendInPlace(bit);
  return ret;  
}
inline BitSeq PopBack(const BitSeq & s) {
  BitSeq ret = s;
  ret.PopBackInPlace();
  return ret;  
}

inline istream & operator>>(istream & input, BitSeq & s) {
  input >> ws;
  CHECK(input.peek() == '#');
  input.get();
  s = BitSeq();
  char c;
  if (input.peek() == 'm') {
    s = BitSeq::Min();
    input >> c;
    return input;
  }
  while (isdigit(input.peek())) {
    input >> c;
    if (c=='1') s.AppendInPlace(true);
    else if (c=='0') s.AppendInPlace(false);
    else CHECK(false);
  }
  return input;
}

inline uint32 Hash32(BitSeq s, uint32 level = 0){
  return Hash32(s.data_, level);
}
DEFINE_HASH_CLASS_0(BitSeq);

/*
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
*/

// A time is a vector in infinite-dimensional space.  The dimensions are labeled
// by BitSeqs, and the coordinates in those dimensions are non-negative
// integers.  Only a finite number of these coordinates are non-zero.
// Dimensions labeled by larger BitSeqs are more significant.  
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
  vector<pair<BitSeq, int> > coordinates_;

  Time() {never_ = false;} // times default to CREATION
  Time(vector<pair<BitSeq, int> > coordinates) 
    :never_(false), coordinates_(coordinates){
    for (uint i=0; i+1<coordinates_.size(); i++) {
      CHECK(coordinates_[i].first > coordinates_[i+1].first);
    }
  }
  string ToString() const;
  void Increment(const BitSeq & coordinate, int count);
  bool IsNever() { return never_;}
  static Time Never() { Time t; t.never_ = true; return t;}
};
inline uint32 Hash32(const Time & t, uint32 seed = 0) {
  return Hash32(t.coordinates_, seed);
}
DEFINE_HASH_CLASS_0(Time);

inline Time operator +(const Time & t, BitSeq coordninate){
  Time ret = t;
  ret.Increment(coordninate, 1);
  return ret;
}

istream & operator >> (istream & input, Time & t);

inline ostream & operator << (ostream & output, const Time &n){
  output << n.ToString();
  return output;
}

inline string TimeToStringOrNothing(const Time *t) {
  if (!t) return "";
  return t->ToString();
}


// #define NEVER Time::Never()
// #define CREATION Time()
bool operator <(const Time & a, const Time & b);
bool operator ==(const Time & a, const Time & b);
inline bool OPERATORGT(const Time & a, const Time & b) { return (b<a);}
inline bool OPERATORLE(const Time & a, const Time & b) { return !(b<a);}
inline bool OPERATORGE(const Time & a, const Time & b) { return !(a<b);}
inline bool operator !=(const Time & a, const Time & b){return !(a==b);}

#endif
