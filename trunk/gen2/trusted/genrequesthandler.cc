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

#include "genrequesthandler.h"
#include "model.h"
#include "element.h"
#include "record.h"


string GenRequestHandler::TopNavHTML() const {
  string ret;
  ret += "<font size=-1>";
  ret += HTMLLink("program", "Program") + " "; 
  for (int i=0; i<Base::NumTypes(); i++) {
    Base::Type t = Base::Type(i);
    ret += HTMLLink("typelist?type=" 
		    + URLEscape(Base::TypeToString(t)),
		    Base::TypeToString(t))
      + "(" + itoa(N.Index(t).size()) 
      + "/" + itoa(N.GetCurrentCount(t)) 
      + "/" + itoa(N.GetAllTimeCount(t)) 
      + ") ";
  }
  ret += HTMLLink(BB.GetURL(), "Blackboard");
  ret += "</font><br><p>";
  return ret;
}

string GenRequestHandler::TypeListHTML(Base::Type type) const {
  string ret;
  if (type < 0 || type >= Base::NumTypes()) {
    return "unkown type " + itoa(type);
  }
  if (type == Base::VIOLATION) {
    for (int i=0; i<Violation::NumTypes(); i++) {
      ret += Violation::TypeToString(Violation::Type(i)) 
	+ " : " +  itoa(Violation::counts_[i])
	+ "<br>\n";
    }
  }
  vector<Record> v;
  forall(run, N.Index(type)) {
    v.push_back(run->second->GetRecordForDisplay());
  }
  ret += RecordVectorToHTMLTable(v);
  return ret;
}

string BlackboardSearchForm(Record params) {
  string ret = "<form action=/blackboard-search>";
  ret += "<br>search pattern( <input name=pattern ";
  ret += "value=\"" + params["pattern"] + "\"";
  ret += "type=text> )";
  ret += " sampling <input name=sampling-info type=text>";
  ret += "<input type=submit></form><br>";
  return ret;
}

string GenRequestHandler::Handle(Record params) {
  string ret = SimpleHTMLHeader() + TopNavHTML();
  string command = params["_command"];
  // display information about a base object. 
  if (command == "getobject") {
    Base *base;
    if (params % string("name")) {
      string sname = params["name"];
      Object name = StringToObject(sname);
      Base::Type type = Base::StringToType(params["type"]);
      base = N.Lookup(type, name);
    } else {
      if (! (params % string("address")) ) {
	ret += "Error - no name or address specified";
	return ret;
      }
      base = (Base*)atop(params["address"]);      
    }
    if (!base) {
      ret += "Object not found";
      return ret;
    }
    // We are overloading the getobject command to also get owned postings.
    // It would be more elegant to have a separate command, but instead
    // we are just tacking on a parameter ownedposting= to signify that 
    // we want the posting owned by the given base object.
    if (params % string("ownedposting")) {
      ret += 
	RecordToHTMLTable(base->GetOwnedPosting()->GetRecordForDisplay());
      return ret;
    }
    ret += "<font size=+1>" + base->ShortDescription() + "</font><p>";
    ret += RecordToHTMLTable(base->GetRecordForDisplay());
    return ret;
  }
  if (command == "typelist") {
    ret += TypeListHTML(Base::StringToType(params["type"]));
    return ret;
  }
  if (command == "tupleinfo") {
    const TupleInfo * ti 
      = BB.GetConstTupleInfo(OTuple(StringToObject(params["tuple"])));
    if (!ti) return ret + "Tuple not found on blackboard";
    ret += RecordToHTMLTable(ti->GetRecordForDisplay());
    return ret;
  }
  if (command == "indexrow") {
    const IndexRow * ir 
      = BB.GetConstIndexRow(OTuple(StringToObject(params["tuple"])));
    if (!ir) return ret + "Index row not found";
    ret += RecordToHTMLTable(ir->GetRecordForDisplay());
    return ret;
  }
  if (command == "blackboard") {
    ret += BlackboardSearchForm(params);
    ret += RecordToHTMLTable(BB.GetRecordForDisplay());
    return ret;
  }
  if (command == "blackboard-search") {
    ret += BlackboardSearchForm(params);
    if (params["pattern"] == "") {
      return ret;
    }
    OPattern p = OPattern(StringToObject("pattern (" + params["pattern"] + ")"));

    if (p == NULL) {
      ret += "error parsing pattern";
      return ret;
    }
    SamplingInfo s = SamplingInfo::Unsampled();
    if (params["sampling-info"] != "") {
      SamplingInfo s = SamplingInfo::StringToSamplingInfo(params["sampling-info"]);
    }
    vector<Map> subs;
    vector<Time> times;
    uint64 num_sats = 0;
    int64 max_work = 1000000;
    bool result = BB.FindSatisfactions(p, s, &subs, &times, &num_sats, &max_work);
    if (!result) {
      ret += "query took too long<br>";
      return ret;
    }
    ret += "#sats: " + itoa(num_sats) + "<br>";
    for (uint c=0; c<subs.size(); c++) {
      OPattern new_pattern = Substitute(subs[c], p);
      ret += new_pattern.ToString() + " " + times[c].ToString() + " <br>\n";
      if (c == 1000) {
	ret += "...<br>";
	break;
      }
    }

  }

  if (command == "program") {
    ret += M.ToString(true);
  }

  return ret;
}




