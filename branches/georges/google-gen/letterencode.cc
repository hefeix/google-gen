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


#include <iostream>
#include <fstream>
#include <ctype.h>

using namespace std;

int main() {
  char c;
  int pos=0;
  cout << "[ Successor START *POS0 ]" << endl;
  while (cin.get(c)){    
    if (pos>0) cout << "[ Successor POS" << pos-1 << " *POS" << pos << " ]"
		    << endl;    
    cout << "[ HasChar POS" << pos << " *Char_";
    if (c=='\n') cout << "newline";
    else if (isspace(c)) cout << "space";
    else cout << c;
    cout << " ]" << endl;
    pos++;
  }
  cout << "[ Successor POS" << pos-1 << " *END ]" << endl;
  return 0;
}
