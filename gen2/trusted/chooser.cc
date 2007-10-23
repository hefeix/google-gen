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

/*
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

#include "chooser.h"
#include "probutil.h"
#include "changelist.h"
#include "model.h"

#undef ITEM
#define ITEM(x) #x

CLASS_ENUM_DEFINE(GlobalChooser, Strategy);

bool GlobalChooser::ChoiceIsPossible(Strategy strategy, Object parameter,
				     Object value) const {
  switch(strategy) {
  case INDEPENDENT_BOOL:
  case LOCAL_BOOL:
    return (value.GetType() == Object::BOOLEAN);
  case LOCAL_UINT:
  case QUADRATIC_UINT:
    return (value.GetType() == Object::INTEGER && Integer(value).Data() >= 0);
  case GLOBAL_FLAKE:
  case LOCAL_FLAKE: 
    return (value.GetType() == Object::FLAKE);
  default:
    return false;
  }
}
void GlobalChooser::L1_ChangeCount(Strategy strategy, Object parameter,
				   Object value, int delta) {
  CHECK(ChoiceIsPossible(strategy, parameter, value));
  if (IsIndependent(strategy)) {
    LL ll_delta 
      = GetIndependentChoiceLnLikelihood(strategy, parameter, value) * delta;
    L1_AddToLnLikelihood(ll_delta);
    return;
  }
  Chooser * c = GetCreateChooser(strategy, parameter);
  c->L1_ChangeCount(value, delta);
  if (c->IsEmpty()) {
    switch(strategy) {
    case LOCAL_FLAKE: 
      c->L1_Erase();
      CL.RemoveFromMap(&local_flake_choosers_, parameter);
      break;
    case LOCAL_BOOL:
      c->L1_Erase();
      CL.RemoveFromMap(&local_bool_choosers_, parameter);
      break;
    case LOCAL_UINT:
      c->L1_Erase();
      CL.RemoveFromMap(&local_uint_choosers_, parameter);
      break;
    default:
      CHECK(false);
    }
  }
}

bool GlobalChooser::IsIndependent(GlobalChooser::Strategy strategy) const { 
  return (strategy < NUM_INDEPENDENT_STRATEGIES);
}
LL GlobalChooser::GetIndependentChoiceLnLikelihood(Strategy strategy, 
						   Object parameter,
						   Object value) const {
  switch(strategy) {
  case INDEPENDENT_BOOL : {
    bool b = Boolean(value).Data();
    double param = Real(parameter).Data();
    return Log(b?param:(1.0-param));
  }
  case QUADRATIC_UINT : {
    int i = Integer(value).Data();
    CHECK(i >= 0);
    return uintQuadraticLnProb(i);
  }
  default:
    CHECK(false);
    break;
  };
  return LL(0);
}

GlobalChooser::GlobalChooser() {
  ln_likelihood_ = LL(0);
  global_flake_chooser_ = New<GenericChooser>(NO_STRATEGY, NULL);
}
Chooser * GlobalChooser::GetCreateChooser(Strategy strategy, Object parameter){
  switch(strategy) {
  case GLOBAL_FLAKE:
    return global_flake_chooser_;
  case LOCAL_FLAKE: {
    GenericChooser ** look = local_flake_choosers_ % parameter;
    if (look) return *look;
    GenericChooser * nu = New<GenericChooser>(GLOBAL_FLAKE, NULL);
    CL.InsertIntoMap(&local_flake_choosers_, parameter, nu);
    return nu;
  }
  case LOCAL_UINT: {
    GenericChooser ** look = local_uint_choosers_ % parameter;
    if (look) return *look;
    GenericChooser * nu = New<GenericChooser>(QUADRATIC_UINT, NULL);
    CL.InsertIntoMap(&local_uint_choosers_, parameter, nu);
    return nu;    
  }
  case LOCAL_BOOL: {
    BooleanChooser ** look = local_bool_choosers_ % parameter;
    if (look) return *look;
    BooleanChooser *nu = New<BooleanChooser>();
    CL.InsertIntoMap(&local_bool_choosers_, parameter, nu);
    return nu;
  }
  default:
    CHECK(false);
    return NULL;
  }
}

void GlobalChooser::L1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, ln_likelihood_ +  delta);
  M.A1_AddToLnLikelihood(delta);
}

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

void Chooser::L1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, ln_likelihood_ +  delta);
  M.A1_AddToLnLikelihood(delta);
}

LL GenericChooser::ComputeLLDelta(Object object,
			   int old_count, int new_count, 
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

void GenericChooser::L1_ChangeCount(Object value, int delta) {

  CHECK (delta != 0);
  
  pair<int, Choice *> * look = counts_ % value;
  int old_count = look ? look->first : 0;
  int new_count = old_count + delta;
  CHECK (new_count >= 0);
  
  int old_num_values = counts_.size();
  int64 old_total = total_choices_;
  
  if (!look) {
    Choice * choice = NULL;
    if (parent_strategy_ != GlobalChooser::NO_STRATEGY) 
      choice = New<Choice>(parent_strategy_, parent_parameter_, value); 
    CL.InsertIntoMap(&counts_, value, make_pair(new_count, choice));
  } else if (new_count == 0) {
    look->second->L1_Erase();
    CL.RemoveFromMap(&counts_, value);
  } else {
    CL.ChangeMapValue(&counts_, value, make_pair(new_count, look->second));
  }
  
  int new_num_values = counts_.size();
  int64 new_total = total_choices_;

  LL ll_delta = ComputeLLDelta(value, 
			       old_count, new_count, old_num_values, 
			       new_num_values, old_total, new_total);
  L1_AddToLnLikelihood(ll_delta);
}

void Chooser::L1_Init() {
  Base::L1_Init();
  ln_likelihood_ = 0;
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
Record GenericChooser::GetRecordForDisplay() const{
  Record r = Chooser::GetRecordForDisplay();
  r["Num Objects"] = itoa(counts_.size());
  if (!include_objects) return r;

  forall (run, counts_) {
    r["objects"] += run->first.ToString() + ":" + itoa(run->second.first) 
      + "<br>\n";
  }
  return r;
}

int Chooser::GetCount(Object object) const {
  const int * find = counts_ % object;
  if (!find) return 0;
  return *find;
}

void Choice::L1_Init(GlobalChooser::Strategy strategy, 
		Object parameter, Object value) {
  CL.Creating(this);
  L1_Change(strategy, parameter, value);
}
void Choice::L1_Erase() {
  L1_AddToChooser(-1);
  CL.Destroying(this);
}

void Choice::L1_AddToChooser(int count_delta) {
  if (value_ == NULL) return;
  GC.L1_ChangeCount(strategy_, parameter_, value_, count_delta);
}
void Choice::L1_Change(GlobalChooser::Strategy new_strategy, 
		       Object new_parameter, Object new_value){
  if (!GlobalChooser.ChoiceIsPossible(new_strategy, new_parameter, new_value)) {
    new_value = NULL;
  }
  L1_AddToChooser(-1);
  CL.ChangeValue(&strategy_, new_strategy);
  CL.ChangeValue(&parameter_, new_parameter);
  CL.ChangeValue(&value_, new_value);
  L1_AddToChooser(1);
}

LL BooleanChooser::ComputeLnLikelihood() const {
  LL ret = -Log(total_choices_+1);
  ret += LnFactorial(total_choices_) 
    - LnFactorial(counts_[0]) - LnFactorial(counts_[1]);
  return ret;
}

void BooleanChooser::L1_ChangeCount(Object value, int delta) {
  int val = (Boolean(value).GetValue())?1:0;
  CL.ChangeValue(&counts_[val], counts_[val] + delta);
  CL.ChangeValue(&total_choices_, counts_[0] + counts_[1]);
  L1_AddToLnLikelihood(ComputeLnLikelihood() - ln_likelihood_);
  CHECK(total_choices_ == counts_[0] + counts_[1]);
}
 
