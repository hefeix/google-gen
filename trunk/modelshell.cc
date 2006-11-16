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
  improvement_counter_ = 0;
  model_filename_ = "";
  model_last_written_ = 0;
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

// This is mostly to view the model
map<string,string> ModelShellHandleExternal(map<string, string> parameters) {

  // Create the modelshell if need be
  static ModelShell * ms = NULL;
  if (!ms) ms = new ModelShell;

  // This is pretty ugly, encapsulate streambuf for cerr
  // then run the command, then rereplace the streambuff
  stringstream cerr_string_stream;
  streambuf *errbuf = cerr.rdbuf(cerr_string_stream.rdbuf());
  string ret = ms->HandleHtml(parameters); // run the command
  cerr.rdbuf(errbuf);

  // Make the return map and return
  map<string, string> retmap;
  retmap["cerr"] = cerr_string_stream.str(); retmap["html"] = ret;
  return retmap;
}

// A helper function
string FilenameEntry() {
  stringstream ret;
  ret << "<form method=get>";
  ret << "Model File:";
  ret << "<input type=text name=filename>";
  ret << "<input type=hidden name=command value=loadmodel>";
  ret << "</form>";
  return ret.str();
}

// This just returns html based on a map
string ModelShell::HandleHtml(map<string, string> params) {

  string command = params["command"];
  stringstream ret;

  // This is the default action, ask for a filename, and show the model
  if (command == "") {
    ret << FilenameEntry();
    command = "showmodel";
  }

  // Load a model
  struct stat statbuf; // for getting when a model was written

  if (command == "loadmodel") {
    model_filename_ = "stored/" + params["filename"] + ".model";
    stat(model_filename_.c_str(), &statbuf);
    if (model_) delete model_;
    model_ = new Model();
    model_->Load(model_filename_);
    model_->VerifyLayer2();
    model_last_written_ = statbuf.st_mtime;
    ret << "Loaded Model <br>";
    command = "showmodel";
  }

  // That's all you can do without a model
  if (model_ == 0) return ret.str();

  // If the model is out of date, reload the model
  if (!(model_filename_ == "")) {
    stat(model_filename_.c_str(), &statbuf);
    if (statbuf.st_mtime != model_last_written_) {
      if (model_) delete model_; 
      model_ = new Model();
      model_->Load(model_filename_); 
      model_->VerifyLayer2();
      model_last_written_ = statbuf.st_mtime;
      ret << "Reloaded Model <br>";
    }
  }

  // Make all the show commands last
  ret << model_->DLinkBar();

  if (command == "showcomponentsoftype") {
    set<ComponentType> ct;
    ct.insert(StringToComponentType(params["type"]));
    model_->ToHTMLByComponentType(ret, ct);
  }

  // Show one component
  if (command == "showcomponent") {
    uint id = atoi(params["id"]);
    Component * c = model_->GetComponent(id);
    if (c) ret << RecordToHTMLTable(c->RecordForDisplay());
  }

  // This should be last :)
  if (command == "showmodel") ret << RecordToHTMLTable(model_->ModelInfo());

  return ret.str();
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
    else if (command == "numsat") {
      Record r;
      command_stream >> r;
      Pattern p = StringToTupleVector(r["pattern"]);
      uint64 num_sat;
      model_->GetTupleIndex()->FindSatisfactions(p, NULL, &num_sat, 
						 UNLIMITED_WORK, NULL);
      cout << "Pattern=" << TupleVectorToString(p)
	   << " num_sat=" << num_sat << endl;
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
	CandidateRule cand; 
	string comments;
	if (!optimizer_->
	    FindRandomCandidateRule(&cand, Tactic(tactic),
				    end_time-time(0), &comments)) break;
	OptimizationCheckpoint cp(optimizer_, true);
	optimizer_->TryAddPositiveRule(cand.first, cand.second, 
					  10, comments);	
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << CandidateRuleToString(cand)
		  << " model likelihood: " << model_->GetLnLikelihood()
		  << " gain=" << cp.Gain() << " " << comments 
		  << endl;
	  model_->ToHTML("html");
	  improvement_counter_++;	  
	  model_->Store("stored/auto."+itoa(improvement_counter_)+".model");
	}
	//model_->VerifyLayer2();
      }
      model_->ToHTML("html");
      
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
      {
	OptimizationCheckpoint cp(optimizer_, true);
	string comments = "Added by hand";
	optimizer_->TryAddPositiveRule(preconditions, result, 10, comments);
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << TupleVectorToString(preconditions)
		  << " ->" << TupleVectorToString(result)
		<< " model likelihood: " << model_->GetLnLikelihood()
		  << " gain=" << cp.Gain() << endl;
	}
      }
      model_->ToHTML("html");
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
	CandidateRule cand;
	string comments;
	if (!optimizer_->FindRandomCandidateRule(&cand, (Tactic)tactic,
						 10, &comments)) break;
	cout << CandidateRuleToString(cand) << endl;
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
