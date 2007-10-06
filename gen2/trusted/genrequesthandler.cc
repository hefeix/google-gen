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
  for (int i=0; i<Named::NumTypes(); i++) {
    Named::Type t = Named::Type(i);
    ret += HTMLLink("typelist?type=" 
		    + URLEscape(Named::TypeToString(t)),
		    Named::TypeToString(t))
      + "(" + itoa(N.Index(t).size()) + "/" +
      itoa(Named::counts_[t]) + ") ";
  }
  ret += HTMLLink(BB.GetURL(), "Blackboard");
  ret += "</font><br><p>";
  return ret;
}

string GenRequestHandler::TypeListHTML(Named::Type type) const {
  string ret;
  if (type < 0 || type >= Named::NumTypes()) {
    return "unkown type " + itoa(type);
  }
  if (type == Named::VIOLATION) {
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


string GenRequestHandler::Handle(Record params) {
  string ret = SimpleHTMLHeader() + TopNavHTML();
  string command = params["_command"];
  // display information about a named object. 
  if (command == "getobject") {
    string sname = params["name"];
    Object name = StringToObject(sname);
    Named::Type type = Named::StringToType(params["type"]);
    Named * named = N.Lookup(type, name);
    if (!named) {
      ret += "Object not found";
      return ret;
    }
    // We are overloading the getobject command to also get owned postings.
    // It would be more elegant to have a separate command, but instead
    // we are just tacking on a parameter ownedposting= to signify that 
    // we want the posting owned by the given named object.
    if (params % string("ownedposting")) {
      ret += 
	RecordToHTMLTable(named->GetOwnedPosting()->GetRecordForDisplay());
      return ret;
    }
    ret += "<font size=+1>" + named->ShortDescription() + "</font><p>";
    ret += RecordToHTMLTable(named->GetRecordForDisplay());
    return ret;
  }
  if (command == "typelist") {
    ret += TypeListHTML(Named::StringToType(params["type"]));
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
    ret += RecordToHTMLTable(BB.GetRecordForDisplay());
    return ret;
  }
  //if (command == "program") {
    ret += M.ToString(true);
    // }
  return ret;
}




