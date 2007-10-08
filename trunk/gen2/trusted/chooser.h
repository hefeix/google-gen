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

#ifndef _CHOOSER_H_
#define _CHOOSER_H_

#include "util.h"
#include "record.h"
#include "probutil.h"
#include "base.h"

// Keeps track of the likelihood of a sequence of choices of objects.  
// Doesn't actually keep track of the sequence.
// A chooser can have a parent chooser.  This means that the different objects
// in this chooser are chosen (once each) by the parent chooser.  Modifying 
// this chooser automatically modifies the parent chooser.
// 
// We use choosers in the model in several places where we need to encode 
// sequences of objects.  The current two places are naming objects in 
// defining rules and preconditions, and dynamic naming choices.  
// In defining rules and preconditions, we use a global chooser associated
// with the model, and in dynamic naming choices, we use one child chooser per
// creative variable per rule.  

struct Chooser : public Base {
  Chooser *parent_;
  LL ln_likelihood_;
  map<Object, int> counts_;
  int64 total_;
  void L1_Init();
  void L1_SetParent(Chooser *parent);
  virtual ~Chooser() {}
  void L1_Erase();
  Base::Type GetBaseType() const { return Base::CHOOSER;}
  virtual LL ComputeLLDelta(Object object,
			    int old_count, int new_count, 
			    int old_num_objects,  int new_num_objects, 
			    int old_total, int new_total);
  void L1_ChangeObjectCount(Object object, int delta);
  //  update the ln likelihood of this object and of the model.
  void L1_AddToLnLikelihood(LL delta); 
  virtual LL ComputeLnLikelihood() const; // from scratch for verification.
  int GetCount(Object object) const;
  Record ChooserInfo(bool include_objects);
};
struct UintChooser : public Chooser {
  void L1_Init() { Chooser::L1_Init();}
  virtual LL ComputeLLDelta(Object object,
			    int old_count, int new_count, 
			    int old_num_objects,  int new_num_objects, 
			    int old_total, int new_total);
  virtual LL ComputeLnLikelihood() const; // from scratch for verification.
};

#endif
