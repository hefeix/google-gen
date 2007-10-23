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
#include "genrequesthandler.h"

class Model : public Base{
 public:

  // ---------- L2 functions ----------

  // Reading things in from an object
  void Load(istream & input); // turns into statements.

  // produces machine and human readable version of the static program.
  // if html is set, produces html
  string ToString(bool html) const;

  void SetBatchMode(bool value) {
    CL.ChangeValue(&batch_mode_, value);
    if (value == false) {
      L1_ProcessDelayedChecks();
    }
  }
  // ---------- const functions ----------

  Base::Type GetBaseType() const { return Base::MODEL;}
  // various html output routines
  Violation * GetViolationOfType(Violation::Type type) const;
  Violation * GetFirstViolation() const;

  // ---------- L1 functions ----------

  Model();
  ~Model();

  // erases the chooser if it becomes empty.
  void L1_AddChoiceToFlakeChooser(Object chooser, Flake f, int count_delta);

  void A1_AddToLnLikelihood(LL delta);
  void A1_AddToSearchWork(int64 delta);

  // throw-away variables (we use negative integers)
  Variable L1_GetNextUniqueVariable();

  struct DelayedCheck {
    Element *element_;
    typedef void (Element::*FuncType)();
    FuncType function_;
    DelayedCheck(Element *e, FuncType f) 
      :element_(e), function_(f) {}
  };
  void L1_AddDelayedCheck(Element *e, DelayedCheck::FuncType f) {
    CL.PushBack(&delayed_checks_, DelayedCheck(e,f));
  }
  void L1_ProcessDelayedChecks() {
    for (uint i=0; i<delayed_checks_.size(); i++) {
      const DelayedCheck & dn = delayed_checks_[i];
      (dn.element_->*(dn.function_))();
    }
    CL.ChangeValue(&delayed_checks_, vector<DelayedCheck>());
  }
  
  // ---------- data ----------

  // delayed checks
  vector<DelayedCheck> delayed_checks_;
  bool batch_mode_;

  // The problem specification
  set<Requirement *> requirements_;
  set<Prohibition *> prohibitions_;

  // The violations
  map<Violation::Type, set<Violation *> > violations_by_type_;
  map<OTime, set<Violation *>, DataCompare<OTime> > violations_by_time_;

  // Statements, Expressions, Dynamic counterparts, and choosers are base.
  // The namer can give you maps of these indexed by name.

  // all base objects by name
  map<Object, Base *> name_index_;
  int next_name_;  

  // throw-away variables (we use negative integers)
  // TODO WORKING, make the parsing handle this!!!
  int next_unique_variable_;

  // likelihood and utility tracking

  // Total of search_work_ for all preconditions.
  LL ln_likelihood_;
  uint64 search_work_; 
  // We prefer models that cost us less work in searching for satisfactions of 
  // preconditions, since they are quicker to reason about.  The number of 
  // units of search work is multiplied by this number and subtracted from the
  // ln_likelihood_ of the model (in nats).  Guess: make this number about 
  // 1/1000
  LL work_penalty_;

  // webserver
  GenRequestHandler request_handler_;
  WebServer * webserver_;
};

inline istream & operator >> (istream & input, Model &m) { 
  m.Load(input); 
  return input;
}
inline ostream & operator << (ostream & output, Model &m) {
  output << m.ToString(false);
  return output;
}

extern Model M;

#endif
