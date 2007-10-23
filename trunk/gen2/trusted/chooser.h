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


// The global choser is a structure that a GENTL program can call to randomly 
// return objects.  
// The GENTL syntax of calling the global chooser is "choose strategy 
// (parameter)" The StaticChoose implements this.  It has one object 
// corresponding to the strategy, and a subexpresion corrsponding to the 
// parameter. 

struct Chooser;
struct GenericChooser;
struct BooleanChooser;

struct GlobalChooser {
  #define GlobalChooserStrategyList {			\
      ITEM(NO_STRATEGY),				\
	ITEM(INDEPENDENT_BOOL),				\
	ITEM(QUADRATIC_UINT),				\
	ITEM(NUM_INDEPENDENT_STRATEGIES),		\
	ITEM(GLOBAL_FLAKE),				\
	ITEM(LOCAL_FLAKE),				\
	ITEM(LOCAL_BOOL),				\
	ITEM(LOCAL_UINT),				\
	};
  CLASS_ENUM_DECLARE(GlobalChooser, Strategy);

  bool ChoiceIsPossible(Strategy s, Object p, Object v) const;

  // This function is what needs to be called externally from the choice object
  void L1_ChangeCount(Strategy strategy, Object parameter, 
		      Object value, int delta);

  // For some strategies, each choice is independent of each other choice.
  // All other strategies are covered by chooser objects. 
  bool IsIndependent(Strategy strategy) const;
  // For independent strategies, gives you the likelihood of a choice. 
  LL GetIndependentChoiceLnLikelihood(Strategy strategy,
				      Object parameter,
				      Object value) const;
  // For non-independent strategies, gives you a pointer to a chooser that
  // that needs to be called to make your choice. 
  Chooser * GetCreateChooser(Strategy strategy, Object parameter);
  //  update the ln likelihood of this object and of the model.
  void L1_AddToLnLikelihood(LL delta); 
  
  GlobalChooser();

  // For flake choosing
  GenericChooser * global_flake_chooser_;
  hash_map<Object, GenericChooser *> local_flake_choosers_;
  hash_map<Object, GenericChooser *> local_uint_choosers_;
  hash_map<Object, BooleanChooser *> local_bool_choosers_;

  LL ln_likelihood_; // for choices not covered in choosers.
};

extern GlobalChooser GC;


struct Choice {
  GlobalChooser::Strategy strategy_;
  Object parameter_;
  Object value_;

  Choice() {value_ = NULL;}
  // In fact, if you try to have an impossible value, these things will just 
  //change value_ to NULL
  void L1_Init(GlobalChooser::Strategy strategy, 
	       Object parameter, Object value);
  void L1_Change(GlobalChooser::Strategy new_strategy, 
		 Object new_parameter, Object new_value);
  /*void L1_ChangeParameter(Object new_parameter) 
  { L1_Change(strategy_, new_parameter, value_);}
  void L1_ChangeValue(Object new_value) 
  { L1_Change(strategy_, parameter_, new_value);}*/
  void L1_Erase();
  private:
  void L1_AddToChooser(int count_delta);
};

// Chooser is an abstract base class for a choosing strategy.
struct Chooser : public Base {
  void L1_Init();
  void L1_Erase();
  Base::Type GetBaseType() const { return Base::CHOOSER;}
  //  update the ln likelihood of this object and of the model.
  void L1_AddToLnLikelihood(LL delta); 
  // This function gets overridden in order to compute the likelihood from 
  // scratch.  It is only for verification purposes.  
  virtual LL ComputeLnLikelihood() const { return ln_likelihood_;}
  virtual void L1_ChangeCount(Object value, int delta);
  bool IsEmpty() const { return (total_choices_ == 0);}
  Record GetRecordForDisplay() const;
  LL ln_likelihood_;
  int64 total_choices_;
};

struct GenericChooser : public Chooser {
  // This is how we choose values to return from this chooser. 
  // if parent_strategy_ == NO_STRATEGY, this is the global flake chooser.
  GlobalChooser::Strategy parent_strategy_;
  Object parent_parameter_;

  // maps from value to count.
  // The Choice * is the choice of the object from the parent strategy.
  hash_map<Object, pair<int, Choice *> > counts_;
  
  void L1_Init(GlobalChooser::Strategy parent_strategy,Object parent_parameter){
    Chooser::L1_Init(); 
    parent_strategy_ = parent_strategy;
    parent_parameter_ = parent_parameter;
  }
  void L1_Erase() {Chooser::L1_Erase();}

  virtual ~GenericChooser() {}
  virtual LL ComputeLLDelta(Object object,
			    int old_count, int new_count, 
			    int old_num_objects,  int new_num_objects, 
			    int old_total, int new_total);
  void L1_ChangeCount(Object value, int delta);
  LL ComputeLnLikelihood() const; // from scratch for verification.
  int GetCount(Object object) const;
  Record GetRecordForDisplay() const;
};

struct BooleanChooser : public Chooser {
  int64 counts_[2];
  void L1_Init() {
    Chooser::L1_Init();
    counts_[0] = counts_[1] = 0;
  }
  void L1_Erase() { Chooser::L1_Erase();}
  LL ComputeLnLikelihood() const;
  void L1_ChangeCount(Object value, int delta);
};







#endif
