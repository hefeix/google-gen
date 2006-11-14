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

#include "modelshell.h"
#include "model.h"
#include "optimization.h"
#include <fstream>

// Just create the model
ModelShell::ModelShell() {
  model_ = new Model();
  optimizer_ = new Optimizer(model_);
}

// Whatever it does, destroy
ModelShell::~ModelShell() {
  delete optimizer_;
  delete model_;
}

void ModelShell::Handle(istream * input) {
  string line;
  while (getline(*input, line)) {
    Handle(line);
  }
}

map<string,string> ModelShellHandleExternal(map<string, string> parameters) {
  static ModelShell * ms = NULL;
  if (!ms) ms = new ModelShell;

  // Pull out the command string
  string command = parameters["command"];

  // This is pretty ugly, encapsulate streambuf for cout
  stringstream cerr_string_stream;
  streambuf *errbuf = cerr.rdbuf(cerr_string_stream.rdbuf());

  // Call the command and get the usual return string
  string ret;
  ret = ms->Handle(command);

  // Rereplace cout stuff
  cerr.rdbuf(errbuf);

  // Now append the return code to cout
  map<string, string> retmap;
  retmap["cerr"] = cerr_string_stream.str();
  return retmap;
}

string ModelShell::Handle(string command) {
  stringstream command_stream;
  command_stream.str(command);

  cout << "?"; cout.flush();
  while (command_stream >> command) {
    if (command == "q") {
      exit(0);
    }
    else if (command=="id") {
      string s;
      command_stream >> s;
      cout << LEXICON.GetAddID(s) << endl;
    }
    else if (command == "RemoveID") {
      int id;
      command_stream >> id;
      model_->GetComponent(id)->Erase();
    }
    else if (command == "verify2") {
      model_->VerifyLayer2();
    }
    /*
    else if (command == "checkpoint") {
      string cname;
      command_stream >> cname;
      checkpoints[cname] = model_->GetChangelist()->GetCheckpoint();
    }
    else if (command == "rollback") {
      string cname;
      command_stream >> cname;
      model_->GetChangelist()->Rollback(checkpoints[cname]);
    }
    */
    else if (command == "spec") {
      string fname;
      command_stream >> fname;
      fname = "spec/" + fname + ".spec";
      ifstream finput(fname.c_str());
      model_->ReadSpec(&finput);
      finput.close();
      model_->VerifyLayer2();
      optimizer_->FixTimesFixCircularDependencies();
      model_->VerifyLayer2();
    }
    else if (command == "strength") {
      int id;
      command_stream >> id;
      optimizer_->OptimizeStrength(model_->GetComponent<Rule>(id));
    }
    /*else if (command == "o") {
      m.OptimizeRound();
      }*/
    else if (command == "i") {
      int tactic;
      command_stream >> tactic;
      int duration;
      command_stream >> duration;
      time_t end_time = time(0) + duration;
      while (time(0) < end_time) {
	pair<vector<Tuple>, vector<Tuple> > p 
	  = optimizer_->FindRandomCandidateRule(Tactic(tactic));
	OptimizationCheckpoint cp(optimizer_, true);
	optimizer_->TryAddImplicationRule(p.first, p.second, 10);	
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << TupleVectorToString(p.first)
		  << " ->" << TupleVectorToString(p.second)
		  << " model likelihood: " << model_->GetLnLikelihood()
		  << " gain=" << cp.Gain() << endl;
	}
	//model_->VerifyLayer2();
      }
    }
    else if (command == "verify"){
      model_->VerifyLayer2();
    }
    else if (command == "addrule"){
      string pat;
      Record r;
      command_stream >> r;
      vector<Tuple> preconditions = StringToTupleVector(r["lhs"]);
      vector<Tuple> result = StringToTupleVector(r["rhs"]);
      OptimizationCheckpoint cp(optimizer_, true);      
      optimizer_->TryAddImplicationRule(preconditions, result, 10);
      if (cp.KeepChanges()) {
	VLOG(0) << " Created rule "
		<< TupleVectorToString(preconditions)
		<< " ->" << TupleVectorToString(result)
		<< " model likelihood: " << model_->GetLnLikelihood()
		<< " gain=" << cp.Gain() << endl;
      }      
    } 
    else if (command=="rs"){ // random tuple
      string l;
      GetLine(command_stream, &l);
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
	  = model_->GetTupleIndex()->GetRandomTupleContaining(terms, true);
	if(s) {
	  cout << s->ToString() << endl;
	}
      }
    }
    else if (command=="v") {
      int v;
      command_stream >> v;
      SetVerbosity(v);
    }
    else if (command=="candidates"){
      uint num;
      int tactic;
      command_stream >> tactic >> num;
      for (uint i=0; i<num; i++) {
	CandidateRule p = optimizer_->FindRandomCandidateRule((Tactic)tactic);
      cout << TupleVectorToString(p.first) << " -> " 
	   << TupleVectorToString(p.second) << endl;
      }
    }
    else if (command=="h"){
      model_->ToHTML("html");
    }
    else if (command=="store") {
      model_->VerifyLayer2();
      system("mkdir -p stored");
      string fn;
      command_stream >> fn;
      fn = "stored/"+fn+".model";
      model_->Store(fn);
    }
    else if (command=="load") {      
      string fn;
      command_stream >> fn;
      fn = "stored/"+fn+".model";
      model_->Load(fn);
      model_->VerifyLayer2();
    }
    else cerr << "UNKNOWN COMMAND " << command << endl;
    model_->FixTimes();
    //ToHTML("/Users/guest/tmp/model.html");
    cout << "?";
  }
  return "DUMMYSTRING\n";
}

int main(int argc, char ** argv) {
  string command;
  if (argc>1) command = argv[1];
  if (command=="tupleindex") {
    TupleIndex index;
    index.Shell();
  } else if (command=="numbers") {
    TestNumbersShell();
  } else if (command =="external") {
    cout << "External Handling\n";
    string line;
    while (getline(cin, line)) {
      map<string, string> params;
      params["command"] = line;
      map<string, string> retmap = ModelShellHandleExternal(params);
      cout << "WOULD OUTPUT " << retmap["cerr"] << endl;
    }
  } else {
    ModelShell ms;
    ms.Handle(&cin);
  }
  return 0;
}