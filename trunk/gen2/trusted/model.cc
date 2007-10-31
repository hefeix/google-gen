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

#include "model.h"
#include "fstream.h"
#include "parser.h"
#include "spec.h"

Model::Model(){
  // for now, we only allow one model to exist. 
  // This cuts down on the pointer chasing.
  CHECK(this==&M); 

  batch_mode_ = false;
  ln_likelihood_ = 0;
  next_name_ = 0;
  next_unique_variable_ = -1;
  webserver_ = new WebServer(&request_handler_);
  webserver_->StartInThread();
  L1_AutomaticallyName();
}

Model::~Model(){}

Variable Model::L1_GetNextUniqueVariable() {
  Variable ret = Variable::Make(next_unique_variable_);
  CL.ChangeValue(&next_unique_variable_, next_unique_variable_-1);
  return ret;
}

void Model::A1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, ln_likelihood_+delta);
}
void Model::A1_AddToSearchWork(int64 delta) {
  CL.ChangeValue(&search_work_, search_work_+delta);
}

void Model::Load(istream &input) {
  OTuple t;
  input >> t;
  if (t == NULL) {
    cerr << "Input file doesn't contain a tuple" << endl;
  }
  //cerr << "Loading from tuple " << t.ToString() << endl;
  vector<Statement *> v = ParseStatements(t.Data());
  //cout << "Just loaded" << endl;
  //for (uint i=0; i<v.size(); i++) cout << v[i]->ToString(0, false);
  //cout << "End program" << endl;
}

string Model::ToString(bool html) const {
  string ret = "{" + GetNewLine(html); 
  forall(run, N.Index(Base::STATEMENT)) {
    Statement *s = dynamic_cast<Statement *>(run->second);
    if (s->parent_ == NULL) {
      ret += s->ToStringRecursive(2);
    }
  }
  ret += "}" + GetNewLine(html);
  if (html) ret = "<tt>" + ret + "</tt>";
  return ret;
}

Violation * Model::GetViolationOfType(Violation::Type type) const {
  const set<Violation *> * s = violations_by_type_ % type;
  if (!s) return NULL;
  return *(s->begin());
}
Violation * Model::GetFirstViolation() const {
  if (violations_by_time_.size() == 0) return NULL;
  return *(violations_by_time_.begin()->second.begin());
}
