// Copyright (C) 2007 Google Inc. and Georges Harik
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
// Author: Georges Harik and Noam Shazeer

#ifndef _MODEL_H_
#define _MODEL_H_

#include "blackboard.h"
#include "element.h"
#include "chooser.h"
#include "violation.h"
#include "record.h"
#include "webserver.h"

class Model : public RequestHandler{
 public:
  Model();
  ~Model();

  // Reading things in from an object
  void Load(istream & input); // turns into statements.
  string ToString() const;
  static void TestLoadAndStore(string filename);
  // erases the chooser if it becomes empty.
  void L1_AddChoiceToFlakeChooser(Object chooser, Flake f, int count_delta);
  
  // handle a web request
  string Handle(Record params);

  // various html output routines
  string TopNavHTML() const;
  string TypeListHTML(Named::Type type) const;

  // The problem specification
  set<Requirement *> requirements_;
  set<Prohibition *> prohibitions_;

  // The violations
  map<Violation::Type, set<Violation *> > violations_by_type_;
  map<OTime, set<Violation *>, DataCompare<OTime> > violations_by_time_;

  // Statements, Expressions, Dynamic counterparts, and choosers are named.
  // The namer can give you maps of these indexed by name.

  // Global Choosers
  Chooser * global_flake_chooser_;
  UintChooser * global_uint_chooser_;

  map<Object, Chooser *> flake_choosers_;

  // all named objects by name
  map<Object, Named *> name_index_;
  int next_name_;  

  // throw-away variables (we use negative integers)
  // TODO WORKING, make the parsing handle this!!!
  int next_unique_variable_;
  Variable L1_GetNextUniqueVariable();

  // likelihood and utility tracking

  // Total of search_work_ for all preconditions.
  void A1_AddToLnLikelihood(LL delta);
  void A1_AddToSearchWork(int64 delta);
  LL ln_likelihood_;
  uint64 search_work_; 
  // We prefer models that cost us less work in searching for satisfactions of 
  // preconditions, since they are quicker to reason about.  The number of 
  // units of search work is multiplied by this number and subtracted from the
  // ln_likelihood_ of the model (in nats).  Guess: make this number about 
  // 1/1000
  LL work_penalty_;

  // webserver
  WebServer * webserver_;

};

inline istream & operator >> (istream & input, Model &m) { 
  m.Load(input); 
  return input;
}
inline ostream & operator << (ostream & output, Model &m) {
  output << m.ToString();
  return output;
}

extern Model M;

#endif
