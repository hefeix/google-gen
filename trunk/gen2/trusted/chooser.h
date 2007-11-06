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
#include "violation.h"

/*

The global chooser is a structure that a GENTL program can call to randomly 
return objects.  
The GENTL syntax of calling the global chooser is "choose (strategy)" 
The StaticChoose implements this.  It has one subexpresion corrsponding to 
 the strategy.

 A strategy is an OTuple whose first element is the strategy type (represented by a keyword) , and the remaining elements represent additional parameters.  An optional name can be included in the tuple to separate it from other strategies with the same type and parameters. 

 Here we document the various strategy types:

--- Independent strategies (ones for which each choice is independent) ---

{ independent_bool, Real prior } 
    Returns TRUE with probability prior and FALSE otherwise.

{ quadratic_uint }
    Returns unsigned integer x with probabilitiy 1/(x+1)(x+2)

{ quadratic_bitseq }
    Returns a bitseq where the length is chosen according to quadratic_uint
    and the bits are chosen uniformly.

--- Choose based strategies ---

{ new_flake }   
   There can only be one such chooser. It cannot have a name.
   This cannot return identified flakes, which have to be returned otherwise
   Should return each invented flake at most once
   Each flake return has probability 1.0. It seems like we aren't properly accounting for the likelihood, but we are, since we only care about getting the right output modulo renaming invented flakes.
   Any flake it returns more than once, has a new flake violation.

{ set_chooser, keyword parent_set_name }
   This is a modeled chooser, which names the set that is it's "parent". Some sets are prepopulated in GEN like "booleans", function_keywords <the set of function keywords>, universal_strategies <the set of universal strategies>, identified_flakes <the set of identified flakes>...

{ generic_chooser, otuple parent_strategy }
   This is a modeled chooser, with its domain being chosen by a parent_strategy

{ meta_chooser, otuple(otuples) strategies }
   A modeled chooser, which makes 2 choices. The first is a choice of strategy, and the second is the object chosen, which is accounted for by the strategy chosen. 

{ universal }
   A particularly brilliant chooser that we will choose to be able to generate anything you could possibly want to generate. We expect this to be used a lot by people who are not flakes.

*/

struct Chooser;
struct GenericChooser;
struct BooleanChooser;
struct Choice;

struct GlobalChooser {
  #define GlobalChooserStrategyTypeList {	       	\
      ITEM(NO_STRATEGY),				\
	ITEM(INDEPENDENT_BOOL),				\
	ITEM(QUADRATIC_UINT),				\
	ITEM(QUADRATIC_BITSEQ),				\
	ITEM(STANDARD_REAL),				\
	ITEM(NUM_INDEPENDENT_STRATEGIES),		\
	ITEM(NEW_FLAKE),				\
        ITEM(SET),                                      \
	ITEM(GENERIC),					\
	ITEM(META),				        \
	};
  CLASS_ENUM_DECLARE(GlobalChooser, StrategyType);

  // const functions
  bool ChoiceIsPossible(OTuple strategy, Object v) const;
  Object RandomChoice(OTuple strategy) const;
  StrategyType GetStrategyType(OTuple strategy) const;
  bool IsIndependent(OTuple strategy) const;

  // For independent strategies, gives you the likelihood of a choice. 
  LL GetIndependentChoiceLnLikelihood(OTuple strategy,
				      Object value) const;
  
  Chooser * GetChooser(OTuple strategy) const;

  void L1_Change(Choice *c, bool adding);
  void L1_Add(Choice *c){ L1_Change(c, true); }
  void L1_Remove(Choice *c){L1_Change(c, false); }

  // For non-independent strategies, gives you a pointer to a chooser that
  // that needs to be called to make your choice. 
  Chooser * L1_GetCreateChooser(OTuple strategy);

  //  update the ln likelihood of this object and of the model.

  void L1_AddToLnLikelihood(LL delta); 
  
  GlobalChooser();

  // ----- This stuff is for metachoices. 
  // suggests a strategy for a metachoice.
  static OTuple SuggestStrategy(Choice *c);
  static OTuple SuggestStrategy(OTuple meta_strategy, Object value);
  void L1_AddMetaChoice (Choice *c, OTuple strategy);
  void L1_RemoveMetaChoice(Choice *c);

  // maps from the metachoice to the choice of strategy and the 
  // choice from that strategy.
  hash_map<Choice *, pair<Choice *, Choice *> > meta_choices_;
  LL ln_likelihood_; // for choices not covered in choosers.
};

extern GlobalChooser GC;

struct Choice : public Base {
  OTuple strategy_;
  Object value_;
  Base *owner_;

  Record GetRecordForDisplay() const;
  string TextIdentifier() const {
    return value_.ToString();
  }

  Base::Type GetBaseType() const { return Base::CHOICE;}
  Choice() { value_ = NULL; }
  // If you try to have an impossible value, we'll set it to NULL
  void L1_Init(Base *owner, OTuple strategy, Object value);
  void L1_Change(OTuple new_strategy, Object new_value);
  void L1_Erase();
};

struct ArbitraryChoice : public Base {
  void L1_Init(Base * owner, LL ln_likelihood, string comment);
  void L1_Erase();
  Base::Type GetBaseType() const { return Base::CHOICE;}
  Record GetRecordForDisplay() const;
  string TextIdentifier() const {
    return "ARBITRARY " + comment_ + " " + ln_likelihood_.ToString();
  }

  Base * owner_;
  string comment_;
  LL ln_likelihood_;
};

// Chooser is an abstract base class for a choosing strategy.
struct Chooser : public Base {
  void L1_Init(OTuple strategy);
  void L1_Erase();

  bool NameMatters() const { return true; }
  bool IsEmpty() const { return (total_choices_ == 0);}
  Base::Type GetBaseType() const { return Base::CHOOSER;}
  Record GetRecordForDisplay() const;

  //  update the ln likelihood of this object and of the model.
  void L1_AddToLnLikelihood(LL delta); 

  // This function gets overridden in order to compute the likelihood from 
  // scratch.  It is only for verification purposes.  
  virtual LL ComputeLnLikelihood() const { return ln_likelihood_;}

  virtual void L1_Change(Choice *c, bool adding);
  
  LL ln_likelihood_;
  int64 total_choices_;

  // choices made from this chooser. 
  hash_map<Object, set<Choice *> > choices_;
};

struct GenericChooser : public Chooser {
  // This is how we choose values to return from this chooser. 
  // if parent_strategy_ == NO_STRATEGY, this is the global flake chooser.
  OTuple parent_strategy_;

  void L1_Init(OTuple strategy){
    Chooser::L1_Init(strategy); 
    parent_strategy_ = OTuple(strategy).Data()[1];
  }
  void L1_Erase() {Chooser::L1_Erase();}

  LL ComputeLLDelta(int old_count, int new_count, 
		    int old_num_objects,  int new_num_objects, 
		    int old_total, int new_total);
  
  void L1_Change(Choice *c, bool adding);
  LL ComputeLnLikelihood() const; // from scratch for verification.
  int GetCount(Object object) const;
  Record GetRecordForDisplay() const;

  // maps from value to count.
  // The Choice * is the choice of the object from the parent strategy.
  hash_map<Object, pair<int, Choice *> > counts_;
};

class SetChooser;

struct ChooserSet : public Base {
  void L1_Init(Object name) {
    Base::L1_Init();
    L1_SetName(name);
  }
  Base::Type GetBaseType() const { return Base::CHOOSER_SET;}

  void L1_Insert(Object o);

  static ChooserSet * FromStrategy(OTuple strategy);
  Record GetRecordForDisplay() const;

  set<Object> set_;
  set<SetChooser *> choosers_;

  static ChooserSet *booleans_;
  static ChooserSet *functions_;
  static ChooserSet *identified_flakes_;
  static ChooserSet *universal_;
  static ChooserSet *misc_;
};

void InitChooserSets();

struct SetChooser : public Chooser {
  void L1_Init(OTuple strategy);
  void L1_Erase();

  LL ComputeLnLikelihood() const;
  void L1_Change(Choice *c, bool adding);
  LL ComputeLLDelta(int old_count, int new_count, 
		    int old_total, int new_total);

  hash_map<Object, int> counts_;
  ChooserSet * my_set_;
};

struct NewFlakeChooser : public Chooser {
  void L1_Init(OTuple strategy){
    Chooser::L1_Init(strategy); 
    CHECK(OTuple(strategy).Data().size() == 1); // no name.
  }
  void L1_Erase() {Chooser::L1_Erase();}
  void L1_Change(Choice *c, bool adding);
  // TODO: the time probably should be something different. 
  OTime GetTime() { return CREATION;} // needed by the violaiton.
  
  map<Flake, Violation *> * GetViolationMap(Violation::Type vtype) {
    return &violations_;
  }
  map<Flake, Violation *> violations_;
};





#endif
