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


#include "lexicon.h"

LexiconWithVariables LEXICON;

Lexicon::Lexicon() {}
Lexicon::~Lexicon() {}
bool Lexicon::GetID(const string & s, int * id) const {
  const int * look = string_to_id_ % s;
  if (look) {
    *id = *look;
    return true;
  } else return false;
}
bool LexiconWithVariables::GetID(const string & s, int * id) const{
  if (s.size() && s[0]=='$') {
    *id = -atoi(s.c_str()+1)-1;
    return true;
  }
  return Lexicon::GetID(s, id);
}
int Lexicon::GetAddID(const string & s) {
  int id;
  if (GetID(s, &id)) return id;
  return Add(s);
}
string Lexicon::GetString(int id) const {
  CHECK(id>=0 && id<(int)id_to_string_.size());
  return id_to_string_[id];
}
string LexiconWithVariables::GetString(int id) const {
  if (id<0) {
    string ret ="$"+itoa(-id-1);
    return ret;
  }
  return Lexicon::GetString(id);
}
int Lexicon::Add(const string & s) {
  int id = id_to_string_.size();
  string_to_id_[s] = id;
  id_to_string_.push_back(s);
  return id;
}

