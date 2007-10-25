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


#include "chooser.h"
#include "probutil.h"
#include "changelist.h"
#include "model.h"

#undef ITEM
#define ITEM(x) #x

CLASS_ENUM_DEFINE(GlobalChooser, StrategyType);

bool GlobalChooser::ChoiceIsPossible(OTuple strategy, 
				     Object value) {

  StrategyType stype = GetStrategyType(strategy);

  switch (stype) {
  case INDEPENDENT_BOOL:
    return (value.GetType() == Object::BOOLEAN);
  case QUADRATIC_UINT:
    return (value.GetType() == Object::INTEGER && Integer(value).Data() >= 0);
  case NEW_FLAKE:
    return (value.GetType() == Object::FLAKE);
  case GENERIC: {
    // Check that its parent can generate its choice
    if (strategy.Data().size() <= 1) {
      cerr << "Error? generic with no parent " << strategy << endl;
      return false;
    }
    OTuple p_strategy = (strategy.Data())[1];
    return ChoiceIsPossible(p_strategy, value);
  }
    // Run through all the meta's strategies and check it
  case META: {
    // TODO: make this not crash if the strategy is malformed
    OTuple strategies = OTuple(strategy.Data()[1]);
    for (uint cc=0; cc < strategies.Data().size(); cc++) {
      if (ChoiceIsPossible(strategies.Data()[cc], value))
	return true;
    }
    return false;
  }
  case SET:
    // TODO need to get the set
    // CRASH
  default:
    return false;
  }
}

GlobalChooser::StrategyType GlobalChooser::GetStrategyType(OTuple strategy) {
  // TODO: make this not crash if the strategy is malformed
  if (strategy.Data().size() == 0) return NO_STRATEGY;
  return StringToStrategyType(Upcase(Keyword(strategy.Data()[0]).Data()));
}

bool GlobalChooser::IsIndependent(OTuple strategy) { 
  return (GetStrategyType(strategy) < NUM_INDEPENDENT_STRATEGIES);
}


void GlobalChooser::L1_Change(Choice *c, bool adding) {
  if (c->value_ == NULL) return;
  CHECK(ChoiceIsPossible(c->strategy_, c->value_));
  
  int delta = adding?1:-1;
  // If it's independent, just account for it and return
  if (IsIndependent(c->strategy_)) {
    LL ll_delta 
      = GetIndependentChoiceLnLikelihood(c->strategy_, c->value_) * delta;
    L1_AddToLnLikelihood(ll_delta);
    return;
  }
  
  // Otherwise make the chooser
  Chooser * chooser = L1_GetCreateChooser(c->strategy_);
  chooser->L1_Change(c, adding);
  
  // Erase the chooser if it becomes empty
  if (chooser->IsEmpty()) chooser->L1_Erase();
}

LL GlobalChooser::GetIndependentChoiceLnLikelihood(OTuple strategy, 
						   Object value) const {
  StrategyType st = GetStrategyType(strategy);

  switch(st) {
  case INDEPENDENT_BOOL : {
    bool b = Boolean(value).Data();
    double param = Real(strategy.Data()[1]).Data();
    return Log(b?param:(1.0-param));
  }
  case QUADRATIC_UINT : {
    int i = Integer(value).Data();
    CHECK(i >= 0);
    return uintQuadraticLnProb(i);
  }
  default:
    CHECK(false);
  };
  return LL(0);
}

Chooser * GlobalChooser::L1_GetCreateChooser(OTuple strategy) {

  // Do we have it already
  Chooser * look = dynamic_cast<Chooser *>(N.Lookup(Base::CHOOSER, strategy));
  if (look) return look;

  StrategyType st = GetStrategyType(strategy);
  switch(st) {
    // case NEW_FLAKE:
    // Make sure it doesn't have a name!
    // CHECK(strategy.Data().size() == 1);
    // return New<NewFlakeChooser>();
  case SET:     return New<SetChooser>(strategy);
  case GENERIC: return New<GenericChooser>(strategy);
    // case META:    return New<MetaChooser>(strategy); 
  default:
    CHECK(false);
    return NULL;
  }
}

void GlobalChooser::L1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, ln_likelihood_ +  delta);
  M.A1_AddToLnLikelihood(delta);
}

GlobalChooser::GlobalChooser() {
  ln_likelihood_ = LL(0);
}

void Choice::L1_Init(Base *owner, OTuple strategy, Object value) {
  owner_ = owner;
  Base::L1_Init();
  L1_Change(strategy, value);
}

void Choice::L1_Change(OTuple new_strategy, Object new_value){
  if (!GlobalChooser::ChoiceIsPossible(new_strategy, new_value))
    new_value = NULL;
  GC.L1_Remove(this);
  CL.ChangeValue(&strategy_, new_strategy);
  CL.ChangeValue(&value_, new_value);
  GC.L1_Add(this);
}

void Choice::L1_Erase() {
  GC.L1_Remove(this);
  Base::L1_Erase();
}

void Chooser::L1_Init(OTuple strategy) {
  Base::L1_Init();
  L1_SetName(strategy);
  ln_likelihood_ = LL(0);
  total_choices_ = 0;
}

void Chooser::L1_Erase(){
  CHECK(ln_likelihood_ == 0);
  CHECK(total_choices_ ==0);
  Base::L1_Erase();
}

Record Chooser::GetRecordForDisplay() const{
  Record r = Base::GetRecordForDisplay();
  r["Ln Likelihood"] = ln_likelihood_.ToString();
  r["Total"] = itoa(total_choices_);
  return r;
}

void Chooser::L1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, ln_likelihood_ +  delta);
  M.A1_AddToLnLikelihood(delta);
}
void Chooser::L1_Change(Choice *c, bool adding) {
  CL.ChangeValue(&total_choices_, total_choices_ + delta);
  if (adding) CL.InsertIntoMapOfSets(&choices_, value, c);
  else CL.RemoveFromMapOfSets(&choices_, value, c);
}
/*
  TODO: This is wrong.   We need to specify how many distinct objects are 
  choosen.  Throw in a term for that.

  To compute the likelihood, we multiply the following:
  1.  The likelihood of the ordering : Prod(count[i]!)/(total!) 
  2.  The number of ways we could have picked these objects
       from the parent distribution: (count.size()!)
  3.  The sequence of object counts:
    since each object occurs at least once, we are allocating 
    (total-count.size()) extra objects among the count.size() objects.
    This can be done using the stars and bars argument with 
    (total - count.size()) objects and (count.size()-1) dividers
    So the number of possibilities is (total-1) choose (count.size()-1).
    a. likelihood = (count.size()-1)! * (total-count.size())! / (total-1)!

  Multiplying it all together, we get:
   Prod(count[i]!) * count.size()! * (count.size()-1)! * (total-count.size())!
    / [ total! * (total-1)! ]

*/

LL GenericChooser::ComputeLnLikelihood() const {
  LL ret(0);
  if (counts_.size()==0) return ret;
  int total = 0;
  forall(run, counts_) {
    int count = run->second.first;
    total += count;
    ret += LnFactorial(count);
  }
  CHECK(total>0);
  CHECK(total == total_choices_);
  ret += LnFactorial(counts_.size());
  ret += LnFactorial(counts_.size()-1);
  ret += LnFactorial(total-counts_.size());
  ret -= LnFactorial(total);
  ret -= LnFactorial(total-1);
  return ret;
}

LL GenericChooser::ComputeLLDelta(int old_count, int new_count, 
				  int old_num_objects, int new_num_objects, 
				  int old_total, int new_total) {
  if (old_total==0 || new_total==0) return 0;
  LL ll_delta = LnFactorial(new_count) - LnFactorial(old_count); 
  ll_delta += LnFactorial(new_num_objects) - LnFactorial(old_num_objects);
  ll_delta += LnFactorial(new_num_objects-1) - LnFactorial(old_num_objects-1);
  ll_delta += LnFactorial(new_total-new_num_objects) 
    - LnFactorial(old_total - old_num_objects);
  ll_delta -= LnFactorial(new_total) - LnFactorial(old_total);
  ll_delta -= LnFactorial(new_total-1) - LnFactorial(old_total-1);
  return ll_delta;
}

void GenericChooser::L1_Change(Choice *c, bool adding) {
  int delta = adding?1:-1;
  const Object & value = c->value_;  
  
  pair<int, Choice *> * look = counts_ % value;
  int old_count = look ? look->first : 0;
  int new_count = old_count + delta;
  CHECK (new_count >= 0);
  
  int old_num_values = counts_.size();
  int64 old_total = total_choices_;
  
  if (!look) {
    Choice * choice = NULL;
    choice = New<Choice>(parent_strategy_, value); 
    CL.InsertIntoMap(&counts_, value, make_pair(new_count, choice));
  } else if (new_count == 0) {
    look->second->L1_Erase();
    CL.RemoveFromMap(&counts_, value);
  } else {
    CL.ChangeMapValue(&counts_, value, make_pair(new_count, look->second));
  }
  Chooser::L1_Change(c, adding);
  
  int new_num_values = counts_.size();
  int64 new_total = total_choices_;

  LL ll_delta = ComputeLLDelta(old_count, new_count, old_num_values, 
			       new_num_values, old_total, new_total);
  L1_AddToLnLikelihood(ll_delta);
}

Record GenericChooser::GetRecordForDisplay() const{
  Record r = Chooser::GetRecordForDisplay();
  r["Num Objects"] = itoa(counts_.size());
  forall (run, counts_) {
    r["objects"] += run->first.ToString() + ":" + itoa(run->second.first) 
      + "<br>\n";
  }
  return r;
}

int GenericChooser::GetCount(Object object) const {
  const pair<int, Choice *> * find = counts_ % object;
  if (!find) return 0;
  return find->first;
}

void ChooserSet::L1_Insert(Object o) {
  CL.InsertIntoSet(&set_, o);
  forall(run, choosers_) {
    // for now, the ChooserSet can't be modified when there are choosers
    // connected.  In the future, they could just recompute their 
    // likelihoods.
    CHECK((*run)->total_choices_ == 0);
  }
}

void InitChooserSets() {
  ChooserSet *booleans = New<ChooserSet>(Keyword::Make("booleans"));
  booleans->L1_Insert(TRUE);
  booleans->L1_Insert(FALSE);

  ChooserSet *functions = New<ChooserSet>(Keyword::Make("functions"));
  for (int i=0; i<Element::NumFunctions(); i++) {
    functions->L1_Insert
      (Keyword::Make(Downcase
		     (Element::FunctionToString(Element::Function(i)))));
  }
}


void SetChooser::L1_Init(OTuple strategy) {
  Chooser::L1_Init(strategy);
  my_set_ = dynamic_cast<ChooserSet *>
    (N.Lookup(CHOOSER_SET, strategy.Data()[1]));
  CHECK(my_set_);
  CL.InsertIntoSet(&my_set_->choosers_, this);
}

void SetChooser::L1_Erase() { 
  CL.RemoveFromSet(&my_set_->choosers_, this);
  Chooser::L1_Erase();
}



/*
  For a SetChooser

  To compute the likelihood, we multiply the following:
  1.  The likelihood of the ordering : Prod(count[i]!)/(total!) 
  2.  The sequence of object counts:
    The objects are not guaranteed to occur at all. 
    We are allocating total choices among the set_size distinct objects.
    This can be done using the stars and bars argument with 
    total stars and (set_size-1) bars.
    So the number of possibilities is (total+set_size-1) choose (set_size-1).
    likelihood = (set_size-1)! * (total)! / (total+set_size-1)!

  Multiplying it all together, we get:
    Prod(count[i]!) * (set_size-1)! / (total+set_size-1)!
   
*/

LL SetChooser::ComputeLnLikelihood() const {
  LL ret(0);
  if (counts_.size()==0) return ret;
  int total = 0;
  forall(run, counts_) {
    int count = run->second;
    total += count;
    ret += LnFactorial(count);
  }
  CHECK(total == total_choices_);
  int set_size = my_set_->set_.size();
  ret += LnFactorial(set_size-1);
  ret += LnFactorial(total+set_size-1);
  return ret;
}

LL SetChooser::ComputeLLDelta(int old_count, int new_count, 
			      int old_total, int new_total) {
  int set_size = my_set_->set_.size();
  if (old_total==0 || new_total==0) return 0;
  LL ll_delta = LnFactorial(new_count) - LnFactorial(old_count); 
  ll_delta += LnFactorial(new_total+set_size-1) 
    - LnFactorial(old_total+set_size-1);
  return ll_delta;
}
 
void SetChooser::L1_Change(Choice *c, bool adding) {
  int delta = adding?1:-1;
  const Object &value = c->value_;
  CHECK(my_set_->set_ % value);
  
  int * look = counts_ % value;
  int old_count = look ? *look : 0;
  int new_count = old_count + delta;
  CHECK (new_count >= 0);
  int64 old_total = total_choices_;
  
  if (!look) {
    CL.InsertIntoMap(&counts_, value, new_count);
  } else if (new_count == 0) {
    CL.RemoveFromMap(&counts_, value);
  } else {
    CL.ChangeMapValue(&counts_, value, new_count );
  }
  Chooser::L1_Change(c, adding);
  
  int64 new_total = total_choices_;

  LL ll_delta = ComputeLLDelta(old_count, new_count, 
			       old_total, new_total);

  L1_AddToLnLikelihood(ll_delta);
}
