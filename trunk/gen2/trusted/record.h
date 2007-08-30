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


#include "util.h"

typedef map<string, string> Record;

// escaped string has no spaces, quotes, whitespace, braces
string RecordEscape(const string & s); 
string RecordUnescape(const string & s);

string RecordToString(const Record &r);
Record StringToRecord(const string & s);

istream & operator >> (istream & input, Record & r);
string RecordToHTMLTable(const Record & r);
string RecordVectorToHTMLTable(const vector<Record> & v);
