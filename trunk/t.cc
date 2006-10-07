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


#include "tupleindex.h"
#include "numbers.h"
#include "model.h"
#include <fstream>
int main(int argc, char ** argv) {
  string command;
  if (argc>1) command = argv[1];
  if (command=="tupleindex") {
    TupleIndex index;
    index.Shell();
  } else if (command=="numbers") {
    TestNumbersShell();
  } else { // if (command=="model") {
    Model model;
    model.Shell(&cin);
  }
}
