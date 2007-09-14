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

  global_flake_chooser_ = New<Chooser>();
  global_uint_chooser_ = New<UintChooser>();
  ln_likelihood_ = 0;
  next_name_ = 0;
  next_unique_variable_ = -1;
  webserver_ = new WebServer(this);
  webserver_->StartInThread();
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
  cerr << "Loading from tuple " << t.ToString() << endl;
  vector<Statement *> v = ParseStatements(t.Data());
  cout << "Just loaded" << endl;
  for (uint i=0; i<v.size(); i++) cout << v[i]->ToString(0, false);
  cout << "End program" << endl;
}

string Model::ToString(bool html) const {
  string ret = "{" + GetNewLine(html); 
  forall(run, N.Index(Named::STATEMENT)) {
    Statement *s = dynamic_cast<Statement *>(run->second);
    if (s->parent_ == NULL) {
      ret += s->ToString(2, html);
    }
  }
  ret += "}" + GetNewLine(html);
  if (html) ret = "<tt>" + ret + "</tt>";
  return ret;
}

void Model::TestLoadAndStore() {
  ifstream inputspec("test_spec");
  LoadSpec(inputspec);
  ifstream input("test_prog");
  input >> M;
  cout << M;
  sleep(10000);
}

void Model::L1_AddChoiceToFlakeChooser(Object chooser_name, 
				       Flake f, int count_delta){
  Chooser * c;
  if (flake_choosers_ % chooser_name) {
    c = flake_choosers_[chooser_name];
  } else {
    c = New<Chooser>();
    c->L1_SetParent(global_flake_chooser_);
    CL.InsertIntoMap(&flake_choosers_, chooser_name, c);
  }
  c->L1_ChangeObjectCount(f, count_delta);
  CHECK(c->GetCount(f) >= 0);
  if (c->total_ == 0) {
    c->L1_Erase();
    CL.RemoveFromMap(&flake_choosers_, chooser_name);
  }
}

string Model::TopNavHTML() const {
  string ret;
  for (uint i=0; i<Named::NUM_NAMED_TYPES; i++) {
    Named::Type t = Named::Type(i);
    string url = "typelist?type=" 
      + URLEscape(Named::TypeToString(t));
    string anchor = Named::TypeToString(t) 
      + "(" + itoa(N.Index(t).size()) + ") ";
    ret += HTMLLink(url, anchor) + " ";
  }
  ret += "<br>";
  return ret;
}

string Model::TypeListHTML(Named::Type type) const {
  string ret;
  if (type < 0 || type >= Named::NUM_NAMED_TYPES) {
    return "unkown type " + itoa(type);
  }
  vector<Record> v;
  forall(run, N.Index(type)) {
    v.push_back(run->second->GetRecordForDisplay());
  }
  ret += RecordVectorToHTMLTable(v);
  return ret;
}

string Model::Handle(Record params) {
  string ret = SimpleHTMLHeader() + TopNavHTML();
  string command = params["_command"];
  if (command == "getobject") {
    string sname = params["name"];
    Object name = StringToObject(sname);
    Named::Type type = Named::StringToType(params["type"]);
    Named * named = N.Lookup(type, name);
    if (named) {
      ret += RecordToHTMLTable(named->GetRecordForDisplay());
    } else {
      ret += "Object not found";
    }
    return ret;
  }
  if (command == "typelist") {
    ret += TypeListHTML(Named::StringToType(params["type"]));
    return ret;
  }
  ret += ToString(true);
  return ret;
}
