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

LL Chooser::ComputeLnLikelihood() const {
  LL ret(0);
  if (counts_.size()==0) return ret;
  int total = 0;
  forall(run, counts_) {
    int count = run->second;
    total += count;
    ret += LnFactorial(count);
  }
  CHECK(total>0);
  CHECK(total == total_);
  ret += LnFactorial(counts_.size());
  ret += LnFactorial(counts_.size()-1);
  ret += LnFactorial(total_-counts_.size());
  ret -= LnFactorial(total_);
  ret -= LnFactorial(total_-1);
  return ret;
}

LL UintChooser::ComputeLnLikelihood() const{
  LL total = 0;
  forall(run, counts_) {
    Integer object = run->first;
    int count = run->second;
    total += count * uintQuadraticLnProb(object);
  }
  return total;  
}

void Chooser::L1_AddToLnLikelihood(LL delta) {
  CL.ChangeValue(&ln_likelihood_, 
		 ln_likelihood_ +  delta);
  M.A1_AddToLnLikelihood(delta);
}

LL Chooser::ComputeLLDelta(Object object,
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

LL UintChooser::ComputeLLDelta(Object object, 
			       int old_count, int new_count,
			       int old_num_objects, int new_num_objects,
			       int old_total, int new_total){
  return (new_count-old_count) * uintQuadraticLnProb(object);
}

void Chooser::L1_ChangeObjectCount(Object object, int delta) {

  CHECK (delta != 0);
  int * look = counts_ % object;
  int old_count = look ? *look : 0;
  int new_count = old_count + delta;
  CHECK (new_count >= 0);
  
  int old_num_objects = counts_.size();
  int64 old_total = total_;
  
  CL.Make(new MapOfCountsAddChange<Object, int>(&counts_, object, delta));
  CL.ChangeValue(&total_, delta + old_total);
  
  int new_num_objects = counts_.size();
  int64 new_total = total_;

  LL ll_delta = ComputeLLDelta(object, 
			       old_count, new_count, old_num_objects, 
			       new_num_objects, old_total, new_total);
  L1_AddToLnLikelihood(ll_delta);

  // Propagate to parent
  if (!parent_) return;
  if (new_count == 0)
    parent_->L1_ChangeObjectCount(object, -1);
  if (old_count == 0)
    parent_->L1_ChangeObjectCount(object, 1);
}

void Chooser::Init() {
  Named::Init();
  parent_ = NULL;
  ln_likelihood_ = 0;
  total_ = 0;
}
void Chooser::L1_SetParent(Chooser *parent) {
  CL.ChangeValue(&parent_, parent);
}

void Chooser::L1_Erase(){
  CHECK(counts_.size()==0);
  CHECK(ln_likelihood_ == 0);
  CHECK(total_ ==0);
  Named::L1_Erase();
}

Record Chooser::ChooserInfo(bool include_objects) {
  Record r;
  r["Ln Likelihood"] = ln_likelihood_.ToString();
  r["Total"] = itoa(total_);
  r["Num Objects"] = itoa(counts_.size());
  if (!include_objects) return r;

  forall (run, counts_) {
    //int object = run->first;
    //int count = run->second;
    // TODO fix all of this file because a chooser should be choosing objects, not ints!!!!
    // r["objects"] += LEXICON.GetString(object) + ":" + itoa(count) + "<br>";
  }
  return r;
}

int Chooser::GetCount(Object object) const {
  const int * find = counts_ % object;
  if (!find) return 0;
  return *find;
}
