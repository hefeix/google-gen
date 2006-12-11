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


#ifndef _LEXICON_H_
#define _LEXICON_H_

#include "util.h"

// A lexicon maps between the non-negative integers and the set of stirngs.
// Use GetAddID to map from string to int (creates IDs on the fly)
// Use GetString to map back to strings
class Lexicon {
 public:
  Lexicon();
  virtual ~Lexicon();

  bool Contains(const string & s) const;                // is a string present
  virtual bool GetID(const string & s, int * id) const; // returns wheter found
  virtual string GetString(int id) const;

  int GetAddID(const string & s);               // adds to lexicon if not found

 private:
  int Add(const string & s);                    // assumes s is not in lexicon.
  vector<string> id_to_string_;
  hash_map<string, int> string_to_id_; 
};

// Maps the negative integers to their ascii representations preceeded by 
// a dollar sign, and the non-negative integers to arbitrary other strings
// using a LEXICON
class LexiconWithVariables : public Lexicon {
 public:
  bool GetID(const string & s, int * id) const;
  string GetString(int id) const;
};

// a global lexicon for our project.  
// TODO: put this in a better place
extern LexiconWithVariables LEXICON;

#endif
