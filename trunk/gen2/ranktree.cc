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

#include "ranktree.h"
 
int main() {
  TestRankSet();
  return 0;

  rankset<string> foo;
  for (int i=0; i<26; i++) {
    string s(string("")+char('a'+((i*17)%26)));
    foo.insert(s);
  }
  
  foo.insert("foo");
  foo.insert("bar");  
  for (int i=0; i<13; i++) {
    string s(string("")+char('a'+i));
    foo.erase(s);
  }
  cout << foo.ToString();
  foo.Check();
  forall(run, foo) {
    cout << (*run) << endl;
  }
  cout << "5th element of foo = " << *(foo.nth(5)) << endl;
  rankset<string>::iterator iter = foo.nth(6);
  rankset<string>::const_iterator iter2 = foo.nth(6);
  const rankset<string> & bar = foo;
  cout << "5th element of foo = " << *(foo.nth(5)) << endl;
  rankset<string>::const_iterator citer = bar.nth(6);
  cout << "DONE" << endl;
  return 0;

}

