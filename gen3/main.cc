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
#include "webserver.h"
#include "genrequesthandler.h"
#include "element.h"
#include "parser.h"

#include <fstream>

void Shell() {
  //TestRankSet();
  //TestRankMap();
  //TestWeightedSet();

  //SetVerbosity(1);
  Tuple main_program;
  Tuple guide_program;
  ReadCodeFile("main.scaffolding", &main_program);
  ReadCodeFile("main_prog", &main_program);

  ReadCodeFile("guide.scaffolding", &guide_program);
  ReadCodeFile("guide_prog", &guide_program);

  Execution *E = Execute(main_program, guide_program, true);
  Blackboard *B = E->blackboard_;
  CHECK(E);
  
  string command;
  while ((cout << "\n?") && (cin >> command)) {
    if (command == "q") break;
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
    if (command == "s") {
      OPattern p;
      cin >> p;
      vector<Map> results;
      uint64 num_sats = 0;
      B->FindSatisfactions(p.Data(), SamplingInfo(), &results, &num_sats, NULL);
      cout << "num results " << results.size() << endl;
      for (uint c=0; c<results.size() && (c<10); c++) {
	cout << results[c] << endl;
      }
    }
    if (command == "rt_match") {
      OTuple rt;
      cin >> rt;
      Tuple t;
      if (B->GetRandomTupleMatching(&t, rt.Data())) {
	cout << t << endl;
      } else {
	cout << "no tuple found" << endl;
      }
    }

    /*if (command == "ai") {
      string comments;
      CandidateRule cr;
      Optimizer::MaybeFindRandomManyExamplesRule(&cr, &comments);
      cout << "rule:" << ToString(cr) << endl;
      }*/
  }
}


int main() {
  rand();
  Object::StaticInit();
  Element::StaticInit();

  N.SetAutomaticallyNameAll(true);
  N.SetTrackCurrentCount(true);

  //ObjectsShell();
  (new WebServer(new GenRequestHandler))->StartInThread();
  //New<Blackboard>()->Shell();

  Shell();
  Object::Destroy();
}
