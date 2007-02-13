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


#include "numbers.h"

string EncodedNumber::ToSortableString() const {
  string ret;
  for (uint i=0; i<bits_.size(); i++) ret+=bits_[i]?'u':'d';
  ret+='e';
  return ret;
}
void  EncodedNumber::FromSortableString(const char ** p) {
  bits_.clear();
  while (**p != 'e') {
    bits_.push_back(**p=='u');
    (*p)++;
  }
  (*p)++;
}

bool (operator==)(const EncodedNumber & a, const EncodedNumber & b) {
  if (a.bits_.size() != b.bits_.size()) return false;
  return (mismatch(a.bits_.begin(), a.bits_.end(), b.bits_.begin()).first 
	  == a.bits_.end());
}

bool OPERATORLESS(const EncodedNumber & a, const EncodedNumber & b) {
  uint min_size = (a.bits_.size() <? b.bits_.size());
  uint num_in_common = 
    mismatch(a.bits_.begin(), a.bits_.begin() + min_size, b.bits_.begin()).first
    - a.bits_.begin();
  if (num_in_common < min_size) {
    return b.bits_[num_in_common];
  }
  if (a.bits_.size() > min_size) return !a.bits_[min_size];
  if (b.bits_.size() > min_size) return b.bits_[min_size];
  return false;
}

string IntToSortableString(int x){
  string s = itoa(x);
  return string(s.size()-1, '_') + s;
}
int IntFromSortableString(const char ** p){
  int length = 1;
  while (**p == '_') {
    (*p)++;
    length++;
  }
  int x = atoi(string(*p, length).c_str());
  (*p) += length;
  return x;
}

string Time::ToSortableString() const {
  if (never_) return "z.NEVER";
  string ret;
  for (uint i=0; i<coordinates_.size(); i++) {
    if (i>0) ret += ',';
    ret += coordinates_[i].first.ToSortableString();
    ret += IntToSortableString(coordinates_[i].second);
  }
  return ret;
}
void Time::FromSortableString(const char ** p){
  coordinates_.clear(); never_ = false;
  if (**p=='z') {never_ = true; return;}
  while ((**p) != '\0'){
    EncodedNumber r;
    r.FromSortableString(p);
    int i = IntFromSortableString(p);
    coordinates_.push_back(make_pair(r, i));
    if ((**p) == ',') *p++;
  }
}

void Time::Increment(const EncodedNumber & coordinate, int  count){
  if (never_) return;
  if (count==0) return;
  vector<pair<EncodedNumber, int> >::iterator look = coordinates_.begin();
  while (look != coordinates_.end() && coordinate < look->first) look++;
  bool coordinate_exists = look != coordinates_.end()
    && look->first == coordinate;
  if (coordinate_exists) {
    look->second += count;
    look++;
  }
  coordinates_.erase(look, coordinates_.end());
  if (!coordinate_exists) coordinates_.push_back(make_pair(coordinate, count));
}
bool OPERATORLESS(const Time & a, const Time & b){
  if (a.never_) return false;
  if (b.never_) return true;
  uint minsize = a.coordinates_.size() <? b.coordinates_.size();
  for (uint i=0; i<minsize; i++) {
    if (a.coordinates_[i].first != b.coordinates_[i].first) {
      if (a.coordinates_[i].first < b.coordinates_[i].first) return true;
      else return false;
    }
    if (a.coordinates_[i].second != b.coordinates_[i].second) {
      if (a.coordinates_[i].second < b.coordinates_[i].second) return true;
      else return false;
    }
  }
  if (a.coordinates_.size() < b.coordinates_.size()) return true;
  return false;
}
bool OPERATOREQ(const Time & a, const Time & b){
  if (a.never_ && b.never_) return true;
  if (a.never_ || b.never_) return false;
  if (a.coordinates_.size() != b.coordinates_.size()) return false;
  for (uint i=0; i<a.coordinates_.size(); i++){
    if (a.coordinates_[i].first != b.coordinates_[i].first) return false;
    if (a.coordinates_[i].second != b.coordinates_[i].second) return false;
  }
  return true;
}

void TestNumbersShell(){
  string command;
  EncodedNumber n;
  Time t;
  while (cin >> command) {
    if (command == "set") {
      string tstring;
      cin >> tstring;
      t.FromSortableString(tstring);
      cout << " t=" << t.ToSortableString() << endl;   
    } else if (command == "increment") {
      string coordinate; int count;
      cin >> coordinate >> count;
      t.Increment(EncodedNumber(coordinate), count);
      cout << " t=" << t.ToSortableString() << endl;
    } else if (command == "inttoss") {
      int i;
      cin >> i;
      cout << IntToSortableString(i) << endl;
    } else if (command == "intfromss") {
      string s;
      cin >> s;
      cout << IntFromSortableString(s) << endl;
    }
  }
}
