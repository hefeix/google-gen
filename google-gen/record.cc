// Copyright (C) 2006 Google Inc.
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
// Author: Noam Shazeer


#include "record.h"
#include <ctype.h>
#include <sstream>

char ToHexit(int d){
  CHECK(d>=0 && d<16);
  if (d<10) return '0'+d;
  return 'A'+(d-10);
}
int FromHexit(char c) {
  if (isdigit(c)) return c-'0';
  return c-'A'+10;
}

string Escape(const string & s){
  ostringstream ret;
  for (uint i=0; i<s.size(); i++) {
    unsigned char c= s[i];
    if ((c=='\"') || (c=='{') || (c=='}') 
	|| (!isascii(c)) || (c=='\\')) {
      ret << '\\' << ToHexit(c/16) << ToHexit(c%16);      
    }
    else ret << c;
  }
  return ret.str();
}
string Unescape(const string & s){
  istringstream istr(s);
  string ret;
  char c;
  while (istr.get(c)) {
    if (c=='\\') 
      ret += char(16*FromHexit(istr.get())+ FromHexit(istr.get()));
    else ret += c;
  }
  return ret;
}

Record StringToRecord(const string & s){
  Record ret;
  istringstream istr(s);
  char c;
  istr >> c >> ws;
  CHECK(c=='{');
  while (istr.peek() != '}') {
    string key, value;
    if (istr.peek()=='\"'){
      while (istr.get(c) && (c!='\"')) key += c;
      istr >> c;
      CHECK(c=='=');
    } else {
      while ((istr >> c) && (c!='=')) key += c;
    }
    istr >> ws;
    if (istr.peek()=='\"'){
      istr.get();
      while (istr.get(c) && (c!='\"')) value += c;
      istr >> c;
    } else {
      while ((istr >> c) && (c!=',') && (c!='}')) value += c;
    }
    ret[Unescape(key)] = Unescape(value);
    if (c=='}') break;
    CHECK(c==',');
  }
  return ret;
}
string RecordToString(const Record &r){
  ostringstream ostr;
  ostr << "{";
  forall(run, r){
    bool quote_first = (run->first.find_first_of("= \t\n")!=string::npos);
    bool quote_second = (run->second.find_first_of(", \t\n")!=string::npos);
    if (run != r.begin()) ostr << ",";
    ostr << (quote_first?"\"":"") << Escape(run->first) 
	 << (quote_first?"\"":"") << "="
	 << (quote_second?"\"":"") << Escape(run->second) 
	 << (quote_second?"\"":"");
  }
  ostr << "}";
  return ostr.str();
}

istream & operator >> (istream & input, Record & r) {	
  char c;
  while (input.get(c) && (c!='{'));
  string s;
  while (input.get(c) && (c!='}')) s+=c;
  r = StringToRecord('{'+s+'}');
  return input;
}
string RecordToHTMLTable(const Record & r){
  ostringstream output;
  output << "<table border=1>" << endl;
  forall(run, r){
    output << "<tr> <td valign=top>" << run->first << "</td><td valign=top>" 
	   << run->second 
	   << "</td></tr>\n";
  }
  output << "</table>\n";
  return output.str();
}
string RecordVectorToHTMLTable(const vector<Record> & v){
  set<string> fields;
  for(uint i=0; i<v.size(); i++) {
    forall(run, v[i]) fields.insert(run->first);
  }
  ostringstream output;
  output << "<table border=1>" << endl;
  for(uint i=0; i<v.size(); i++) {
    if (i%10==0) {
      output << "<tr>";
      forall(run,fields){
	output << "<td valign=top><b>" << *run << "</b></td>";
      }
      output << "</tr>" << endl;
    }
    output << "<tr>";
    forall(run,fields){
      output << "<td valign=top>";
      const string * value = v[i]%(*run);
      if (value) output << *value;
      output << "</td>";
    }
    output << "</tr>" << endl;
  }
  output << "</table>" << endl;
  return output.str();
}
