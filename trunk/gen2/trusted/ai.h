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


#ifndef _AI_H_
#define _AI_H_

#include "violation.h"
#include "model.h"
#include "element.h"

struct PatternBuilder {
  PatternBuilder() {}
  PatternBuilder(const MPattern& p, const vector<Map>& subs) {
    pattern_ = p;
    subs_ = subs;
  } 

  bool TryInitializeFromTuple();

  bool TryExpandOnce();
  bool TryExpandFully(uint size);
  
  void CollapseEquivalentVariables();
  void CollapseConstantVariables();

  string ToString();

  MPattern pattern_;
  vector<Map> subs_;
  map<set<Object>, int> anchor_sets_tried_;
};

struct Optimizer{
  static bool 
  MaybeFindRandomManyExamplesRule(CandidateRule * ret, 
				  string *comments);
  
  static bool
  FindSampling(const MPattern & p, SamplingInfo * result, 
	       int64 max_work,
	       vector<Map> * subs,
	       uint64 * estimated_num_results,
	       uint64 * actual_num_results,
	       set<uint> * bad_clauses, // don't sample these
	       SamplingInfo *hint);
  static bool
  VetteCandidateRule(CandidateRule r, 
		     CandidateRule * simplified_rule, 
		     int64 max_work, string *comments);
  
};


#endif
