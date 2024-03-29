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
  max_recursion_ = 1;
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
    if (c) ret << RecordToHTMLTable(c->RecordForDisplay(true));
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
    else if (command == "sat") {
      Record r;
      command_stream >> r;
      Pattern p = StringToTupleVector(r["pattern"]);
      SamplingInfo sampling;
      if (r["sample"] != "") {
	sampling = SamplingInfo::StringToSamplingInfo(r["sample"]);
      }
      uint64 num_sat;
      vector<Substitution> * subs = NULL;
      uint num_show = 0;
      if (r["show"] != "") {
	num_show = atoi(r["show"]);
	if (num_show)
	  subs = new vector<Substitution>();
      }
      model_->GetTupleIndex()->
	FindSatisfactions(p, sampling, subs, 
			  &num_sat, NULL);
      cout << "Pattern=" << TupleVectorToString(p)
	   << " num_sat=" << num_sat << endl; 
      if (num_show) {
	for (uint c=0; c<num_show && c<subs->size(); c++) {
	  cout << (*subs)[c].ToString() << endl;
	}
	delete subs;
      }
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
    /*else if (command == "o") {
      m.OptimizeRound();
      }*/
    else if (command == "combinerules") {
      OptimizationCheckpoint cp(optimizer_, true);
      int duration;
      command_stream >> duration;
      time_t end_time = time(0) + duration;
      string comments;
      optimizer_->CombineRules(end_time-time(0), &comments);
      if (cp.KeepChanges()) {
	VLOG(0) << "Combined rules" << endl;
      }
    }
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
				       max_recursion_, comments, true);
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << CandidateRuleToString(cand)
		  << " model likelihood: " << model_->GetLnLikelihood()
		  << " gain=" << cp.Gain() << " " << comments 
		  << endl;
	  model_->ToHTML("html");
	  improvement_counter_++;	  
	  VLOG(0) << "About to store improvement\n";
	  model_->Store("stored/auto."+itoa(improvement_counter_)+".model");
	  string cmd = "ln -sf auto." + 
	    itoa(improvement_counter_) + ".model stored/working.model";
	  system(cmd.c_str());
	}
      }
      model_->ToHTML("html");
    }
    else if (command == "verify"){
      model_->VerifyLayer2();
    }
    // Here's an example
    // addrule { lhs="[ Successor *0 *1 ]" , rhs = "[ Successor *1 *2 ]"  }
    else if (command == "componentcheck") {
      Pattern p;
      Record r;
      command_stream >> r;
      p = StringToTupleVector(r["p"]); 
      if (IsConnectedPattern(p)) {
	VLOG(0) << "Connected" << endl;
      } else {
	VLOG(0) << "Not Connected" << endl;
      }
    }
    else if (command == "benefit") {
      Record r;
      command_stream >> r;
      Tuple tp;
      tp.FromString(r["tuple"]);
      const TrueTuple * tt = *((model_->GetTupleToTrueTuple()) % tp);
      if (tt) {
	VLOG(0) << "guess: " << optimizer_->GuessBenefit(tt) << endl;
      } else {
	VLOG(0) << "Can't find tuple" << endl;
      }
    }
    else if (command == "autobenefit") {
      int count;
      command_stream >> count;
      for (uint c=0; c < (uint)count; c++) {
	TrueTuple * tt = optimizer_->GetRandomTrueTuple();
	VLOG(0) << "Tuple:" << tt->GetTuple().ToString() << endl;
	VLOG(0) << "guess: " << optimizer_->GuessBenefit(tt) << endl;
       }
    }
    else if (command == "addrule"){
      string pat;
      Record r;
      command_stream >> r;
      CandidateRule original = make_pair(
 					 StringToTupleVector(r["lhs"]),
 					 StringToTupleVector(r["rhs"]));
      {
 	OptimizationCheckpoint cp(optimizer_, true);
 	string comments;
 	CandidateRule simplified;
 	bool success = optimizer_->VetteCandidateRule
	  (original, &simplified, 
	   optimizer_->ConstantExpectationMaxWork(), 
	   &comments);
 	comments += " added by hand ";
 	cerr << "Vette " << (success?"succeeded":"failed") << endl;
 	if (!success) return "Vette failed";
 	if (success) original = simplified;
 	optimizer_->TryAddPositiveRule(original.first, original.second, 
 				       max_recursion_, comments, false);
 	if (cp.KeepChanges()) {
 	  VLOG(0) << " Created rule "
 		  << CandidateRuleToString(original)
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
 	Tuple s;
 	bool found_random 
 	  = model_->GetTupleIndex()->GetRandomTupleContaining(&s, terms, true);
 	if (found_random) {
 	  cout << s.ToString() << endl;
 	}
       }
     }
     else if (command=="v") {
       int v;
       command_stream >> v;
       SetVerbosity(v);
     }
     else if (command=="fv") {
       int v;
       string f;
       command_stream >> f >> v;
       SetVerbosity(f, v);
     }
     else if (command=="vi"){
       int v;
       command_stream >> v;
       model_->verify_interval_ = v;
       model_->verify_counter_ = 0;
     }
     else if (command=="r"){
       int r;
       command_stream >> r;
       max_recursion_ = r;
     }
     else if (command=="vette") {
       Record r;
       command_stream >> r;
       CandidateRule original = make_pair(
 					 StringToTupleVector(r["lhs"]),
 					 StringToTupleVector(r["rhs"]));
       CandidateRule simplified;
       string comments;
       bool result = optimizer_->VetteCandidateRule(original, &simplified,
						    optimizer_->ConstantExpectationMaxWork(),
						    &comments);
       if (result) {
	 cout << "Simplified " << CandidateRuleToString(simplified) << endl;
       } else {
	 cout << "Failed" << endl;
       }
     }
     else if (command=="candidates"){
       uint num;
       int tactic;
       command_stream >> tactic >> num;
       for (uint i=0; i<num; i++) {
 	CandidateRule cand;
 	string comments;
 	if (!optimizer_->FindRandomCandidateRule(&cand, (Tactic)tactic,
 						 100000, &comments)) break;
        cout << "Found Candidate" <<  CandidateRuleToString(cand) << endl;
       }
     }
     else if (command=="h"){
       model_->ToHTML("html");
       model_->Store("stored/working.model");
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
    // ToHTML("/Users/guest/tmp/model.html");
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
