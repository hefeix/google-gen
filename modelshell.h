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

#ifndef _MODELSHELL_H_
#define _MODELSHELL_H_

#include "util.h"
#include "model.h"
#include "optimization.h"

// This is used to give external commands to a model and receive information
// about a model

class ModelShell {
 public:
  // Create and destroy
  ModelShell();
  ~ModelShell();

  // Various handling commands you can call
  void Handle(istream * input);
  string Handle(string command);

 private:

  // This is the model we're working on
  Model * model_;
  Optimizer * optimizer_;
  int improvement_counter_;
};

map<string, string> ModelShellHandleExternal(map<string, string> parameters);

#endif
