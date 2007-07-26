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

Model::Model(){
  global_flake_chooser_ = new Chooser(NULL);
  global_uint_chooser_ = new UintChooser();
  ln_likelihood_ = 0;
  next_name_ = 0;
  next_unique_variable_ = -1;
}

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
  vector<Statement *> v = Statement::Parse(t.Data());
  cout << "Just loaded" << endl;
  for (uint i=0; i<v.size(); i++) cout << v[i]->ToString(0);
  cout << "End program" << endl;
}

string Model::ToString() const {
  string ret = "{\n";
  forall(run, N.Index(Named::STATEMENT)) {
    Statement *s = dynamic_cast<Statement *>(run->second);
    if (s->parent_ == NULL) {
      ret += s->ToString(2);
    }
  }
  ret += "}\n";
  return ret;
}

void Model::TestLoadAndStore(string filename) {
  ifstream input(filename.c_str());
  input >> M;
  cout << M;
}
