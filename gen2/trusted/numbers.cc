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
#include "objects.h"

string Time::ToString() const {
  if (never_) return "never";
  Map t;
  for (uint i=0; i<coordinates_.size(); i++) {
    t[OBitSeq::Make(coordinates_[i].first)] 
      = Integer::Make(coordinates_[i].second);
  }
  string ret = "time";
  OTuple ot = OMap::Make(t);
  ret += ot.ToString();
  return ret;
}

void Time::Increment(const BitSeq & coordinate, int  count){
  if (never_) return;
  if (count==0) return;
  vector<pair<BitSeq, int> >::iterator look = coordinates_.begin();
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

istream & operator >> (istream & input, Time & t) {
  OTime ot;
  input >> ot;
  t = ot.Data();
  return input;
}
