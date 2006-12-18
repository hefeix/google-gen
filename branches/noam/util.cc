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


#include "util.h"
#include <stdlib.h>
#include <math.h>
#include <sstream>

int VERBOSITY = 0;
int GetVerbosity(){
  return VERBOSITY;
}
void SetVerbosity(int v){
  VERBOSITY = v;
}

bool GetLine(istream & input, string * ret) {
  *ret = "";
  char c;
  if (!input.get(c)) return false;
  while (c!='\n') {
    *ret += c;
    if (!input.get(c)) break;
  }
  return true;
}

string StripWhiteEnds(const string & s){
  int start = 0;
  while (start<(int)s.size() && isspace(s[start])) start++;
  int end = s.size()-1;  
  while (end>start && isspace(s[end-1])) end--;
  return s.substr(start, end-start);
}

vector<string> Split(const string & s, char delim){
  vector<string> ret;
  string current;
  for (uint i=0; i<s.size(); i++) {
    if (s[i]==delim) {
      if (current.size()) ret.push_back(current);
      current = "";
    } else current += s[i];
  }
  if (current.size()) ret.push_back(current);
  return ret;
}
string Join(const vector<string> v, char delim){
  string ret;
  for (uint i=0; i<v.size(); i++) {
    if (i) ret += delim;
    ret += v[i];
  }
  return ret;
}
uint32 RandomUInt32(){
  CHECK(RAND_MAX == 0x7FFFFFFF);
  return (rand() << 1) + rand()%2;
}
double TwoToTheThirtyTwo = pow(2, 32);
double TwoToTheMinusThirtyTwo = pow(0.5, 32);
double OneOverRandMax = (1.0 / RAND_MAX);
double RandomFraction(){
  return (rand() + 0.5) * OneOverRandMax;
}

string IntVectorToString(const vector<int> v){
  ostringstream ostr;
  for (uint i=0; i<v.size(); i++) {
    if (i!=0) ostr << " ";
    ostr << v[i];
  }
  return ostr.str();
}
vector<int> StringToIntVector(const string & s){
  vector<int> ret;
  istringstream istr(s);
  int i;
  while (istr >> i) ret.push_back(i);
  return ret;
}

PermutationIterator::PermutationIterator(int num_items, int num_slots){  
  num_slots_ = num_slots;
  num_items_ = num_items;
  if (num_items_ > num_slots_) {
    done_ = true;
    return;
  }
  slot_to_item_ = vector<int>(num_slots_, EMPTY_SLOT);
  for (int i=0; i<num_items_; i++) {
    slot_to_item_[i] = i;
    item_to_slot_.push_back(i);
  }
  done_ = false;
}
void PermutationIterator::Move(int item, int new_slot) {
  CHECK(new_slot >=0 && new_slot < num_slots_);
  int old_slot = item_to_slot_[item];  
  if (new_slot==old_slot) return;
  int displaced_item = slot_to_item_[new_slot];
  if (displaced_item >=0) item_to_slot_[displaced_item] = EMPTY_SLOT;
  item_to_slot_[item] = new_slot;
  slot_to_item_[new_slot] = item;
  if (old_slot >= 0) slot_to_item_[old_slot] = EMPTY_SLOT;
}
void PermutationIterator::operator++(){
  CHECK(!done_);
  int item_to_advance = num_items_-1;
  int new_slot = 0;
  while (true) {
    if (item_to_advance < 0) {
      done_ = true;
      return;
    }
    new_slot = item_to_slot_[item_to_advance]+1;
    while (new_slot < num_slots_ && 
	   (slot_to_item_[new_slot]>=0 &&
	    slot_to_item_[new_slot]<item_to_advance)) new_slot++;
    if (new_slot < num_slots_) break;
    item_to_advance--;
  }
  Move(item_to_advance, new_slot);
  int pos = 0;
  for (int item=item_to_advance+1; item<num_items_; item++){
    while (slot_to_item_[pos]>=0 && slot_to_item_[pos]<item) pos++;
    Move(item, pos);
  }
}

ProductIterator::ProductIterator(vector<uint> bounds){
  bounds_ = bounds;
  done_ = false;
  current_ = vector<uint>(bounds_.size());
  for (uint i=0; i<bounds_.size(); i++) {
    if (bounds_[i]==0) {
      done_ = true;
      return;
    }
  }
}
void ProductIterator::operator++(){
  uint dim = 0;
  while(1) {
    if (dim >= bounds_.size()){
      done_ = true;
      return;
    }
    current_[dim]++;
    if (current_[dim] == bounds_[dim]) {
      current_[dim] = 0;
      dim++;
    } else {
      break;
    }
  }
}

// returns a vector of size num_objects with densely packed component ids.
// precondition: adjacency_matrix is symmetric
int ConnectedComponents(int num_objects, 
			const map<int, set<int> > & adjancency_matrix,
			vector<int> * components){
  vector<int> c(num_objects, -1);
  int num_components = 0;
  for (int root=0; root<num_objects; root++) {
    if (c[root]!=-1) continue;
    int component = c[root] = num_components;
    num_components++;
    set<int> new_members;
    new_members.insert(root);
    while(new_members.size()){
      int to_add = *(new_members.begin());
      new_members.erase(to_add);
      c[to_add] = component;
      const set<int> * neighbors = adjancency_matrix % to_add;
      if (neighbors) forall(run, *neighbors) {
	if (c[*run] == -1) new_members.insert(*run);
      }
    }
  }
  if (components) *components = c;
  return num_components;
}
