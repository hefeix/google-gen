// Copyright (C) 2006 Google Inc. and Georges Harik
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
// Author: Noam Shazeer and Georges Harik

#include "model.h"
#include "optimization.h"
#include <iostream>
#include <fstream>
#include <sstream>

void ModelShell(istream  * input) {
  Model m;
  string line;
  string command;
  map<string, Checkpoint> checkpoints;
  cout << "?";
  while ((*input) >> command) {
    if (command == "q") break;
    else if (command=="id") {
      string s;
      (*input) >> s;
      cout << LEXICON.GetAddID(s) << endl;
    }
    else if (command == "RemoveID") {
      int id;
      (*input) >> id;
      m.GetComponent(id)->Erase();
    }
    /*else if (command=="store") {
      string filename;
      (*input) >> filename;
      StoreToFile(filename);
    }
    else if (command=="load") {
      string filename;
      (*input) >> filename;
      ifstream finput(filename.c_str());
      Load(&finput);
      }*/
    else if (command == "verify2") {
      m.VerifyLayer2();
    }
    else if (command == "checkpoint") {
      string cname;
      (*input) >> cname;
      checkpoints[cname] = m.GetChangelist()->GetCheckpoint();
    }
    else if (command == "rollback") {
      string cname;
      (*input) >> cname;
      m.GetChangelist()->Rollback(checkpoints[cname]);
    }
    else if (command == "spec") {
      string fname;
      (*input) >> fname;
      ifstream finput(fname.c_str());
      m.ReadSpec(&finput);
      finput.close();
      FixTimesFixCircularDependencies(&m);
    }
    else if (command == "strength") {
      int id;
      (*input) >> id;
      OptimizeStrength(m.GetComponent<Rule>(id));
    }
    /*else if (command == "o") {
      m.OptimizeRound();
      }*/
    else if (command == "i") {
      int tactic;
      (*input) >> tactic;
      int duration;
      (*input) >> duration;
      time_t end_time = time(0) + duration;
      while (time(0) < end_time) {
	pair<vector<Tuple>, vector<Tuple> > p 
	  = FindRandomCandidateRule(&m, Tactic(tactic));
	OptimizationCheckpoint cp(&m, true);
	TryAddImplicationRule(&m, p.first, p.second);
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << TupleVectorToString(p.first)
		  << " ->" << TupleVectorToString(p.second)
		  << " model likelihood: " << m.GetLnLikelihood()
		  << " gain=" << cp.Gain() << endl;
	}
      }
    }
    else if (command == "ispecific"){
      string pat;
      GetLine((*input), &pat);
      vector<Tuple> preconditions = StringToTupleVector(pat);
      GetLine((*input), &pat);
      vector<Tuple> result = StringToTupleVector(pat);
      OptimizationCheckpoint cp(&m, true);      
      TryAddImplicationRule(&m, preconditions, result);
      if (cp.KeepChanges()) {
	VLOG(0) << " Created rule "
		<< TupleVectorToString(preconditions)
		<< " ->" << TupleVectorToString(result)
		<< " model likelihood: " << m.GetLnLikelihood()
		<< " gain=" << cp.Gain() << endl;
      }      
    } 
    else if (command=="rs"){ // random tuple
      string l;
      GetLine((*input), &l);
      istringstream istr(l);
      vector<int> terms;      
      string w;
      while (istr >> w){
	int wid;
	CHECK(LEXICON.GetID(w, &wid));
	terms.push_back(wid);
      }
      for (int i=0; i<10; i++) {
	const Tuple * s 
	  = m.GetTupleIndex()->GetRandomTupleContaining(terms, true);
	if(s) {
	  cout << s->ToString() << endl;
	}
      }
    }
    else if (command=="v") {
      int v;
      (*input) >> v;
      SetVerbosity(v);
    }
    else if (command=="pat"){
      /*pair<vector<Tuple>, vector<Tuple> > p = FindRandomCandidateRule();
      cout << TupleVectorToString(p.first) << " -> " 
      << TupleVectorToString(p.second) << endl;*/
    }
    else if (command=="h"){
      m.ToHTML("html");
    }
    else cerr << "UNKNOWN COMMAND " << command << endl;
    m.FixTimes();
    //ToHTML("/Users/guest/tmp/model.html");
    cout << "?";
  }
}

int main() {
  ModelShell(&cin);
  return 0;
}
