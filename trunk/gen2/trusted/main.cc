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

#include "objects.h"
#include "blackboard.h"
#include "model.h"
#include "spec.h"
#include "fixers.h"
#include "chooser.h"
#include "ai.h"

#include <fstream>

void Shell() {
  ifstream inputspec("test_spec");
  LoadSpec(inputspec);
  ifstream input("test_prog");
  input >> M;
  cout << M;
  StaticExecutor::Execute();
  string command;
  while ((cout << "\n?") && (cin >> command)) {
    if (command == "q") break;
    if (command == "run") {
      StaticExecutor::Execute();
    }
    if (command == "v") {
      int level;
      cin >> level;
      SetVerbosity(level);
    }
    if (command == "fv") {
      int level;
      string func_name;
      cin >> func_name >> level;
      SetVerbosity(func_name, level);
    }
    if (command == "ai") {
      string comments;
      CandidateRule cr;
      Optimizer::MaybeFindRandomManyExamplesRule(&cr, &comments);
      cout << "rule:" << ToString(cr) << endl;
    }
  }
}

int main() {
  rand();
  InitConstants();
  InitChooserSets();
  N.SetAutomaticallyNameAll(true);
  N.SetTrackCurrentCount(true);
  //ObjectsShell();
  // Blackboard::Shell();
  Shell();
  DestroyConstants();
}
