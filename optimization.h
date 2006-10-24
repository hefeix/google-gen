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


#ifndef _OPTIMIZATION_H_
#define _OPTIMIZATION_H_

#include "model.h"

// OPTIMIZATION functions. 
// All functions starting with "Try" have built-in roll-back mechaisms
// and return true if the changes are kept. 
// You can specify what the rollback criterion is, and whether the function
// should bother to fix the times.
bool TryRemoveFiring(Model *m,
		     Firing * f, RollbackCriterion criterion, bool fix_times);
bool TryRemoveRule(Model *m, 
		   Rule * r, RollbackCriterion criterion, bool fix_times);
bool TryAddFirings(Model *m,
		   Rule * r, const vector<Substitution> & sub, 
		   RollbackCriterion criterion, bool fix_times);
// Given a creative rule, tries creating rules where the unbound RHS variables
// in the original rule are replaced by constants.
bool TrySpecifyCreativeRule(Model *m,
			    Rule * r, RollbackCriterion criterion, 
			    bool fix_times);
// TODO, document this
bool TryAddImplicationRule(Model *m,
			   const vector<Tuple> & preconditions, 
			   const vector<Tuple> & result, 
			   RollbackCriterion criterion, bool fix_times);

  
enum Tactic {
  NEW_RULE,
  SPECIFY_ONE,
  GENERALIZE_ONE,
  GENERALIZE_CONNECT,
};

CandidateRule FindRandomCandidateRule(Model *m, Tactic tactic);
bool MaybeFindRandomCandidateRule(Model *m, CandidateRule * ret, Tactic tactic);
bool VetteCandidateRule(Model *m, CandidateRule r, 
			CandidateRule * simplified_rule, 
			int64 max_work);

bool MaybeFindRandomNewRule(Model *m, CandidateRule * ret);
bool MaybeFindRandomVariantRule(Model *m, CandidateRule *ret, Tactic tactic);

// OPTIMIZATION STEPS
void OptimizeRound(Model *m);

TrueTuple * GetRandomTrueTuple(Model *m);

ComputationResult DependsOn(Component * dependent, Component * dependee, 
			    int64 max_work);

// determines whether deleting this component will (recursively) remove
// any of the required true propositions.  
virtual ComputationResult IsEssential(Component *c, int max_work, 
				      int * actual_work);

// Would component c go away, directly or indirectly, if we removed 
// Component d?  The function doesn't recursively inspect, so sometimes
// it is not sure whether there is an indirect dependency and it can
// return RESULT_MAYBE.
ComputationResult RequiresCodependent(Component * dependent, 
				      Component * codependent);
  

  // Sometimes we want to roll back if the model has gotten less likely, and
  // sometimes we only want to roll back if the model is invalid. 
  enum RollbackCriterion {
    REQUIRE_BETTER,
    REQUIRE_VALID,
  };
// This is an object that can be created, which will set a checkpoint upon
// creation, and automatically roll back the model if it doesn't like the
// model when it goes out of scope.  
struct OptimizationCheckpoint { 
  Model * model_;
  Checkpoint cp_;
  double old_ln_likelihood_;
  RollbackCriterion criterion_;
  // should we make sure all of the times are correct in the model
  // before deciding whether to roll back 
  bool fix_times_;  
  OptimizationCheckpoint(Model *model, RollbackCriterion criterion,
			 bool fix_times);
  ~OptimizationCheckpoint();
  // Should we keep the changes
  bool KeepChanges();
  // The increase in model ln likelihood since creation.
  double Gain();
  private:
  // Do we think that the model has gotten better since creation.
  bool Better();
};
  
void OptimizeStrength(Rule *r);

  // Takes an unexplained TrueTuple, and finds the most probable firing 
  // to add to the model that will explain that TrueTuple.  
  // If an "excluded" set is given, we avoid adding firings which depend on
  // the excluded TrueTuples.
void Explain(Model *m,
	     TrueTuple * p,
	     set<Component *> * excluded,
	     bool fix_times); // satisfies in the best way it can
// Explain all of the required propositions.
void FulfillRequirements(Model *m); // call on an empty model
// Fixes the times and deletes all components whose time is NEVER
void FixTimesFixCircularDependencies(Model *m);

// Finds possible pairs of rules and substitutions such that the 
// preconditios are satisfied, and one of the results is the given tuple.
// You can exclude some TrueTuples as dependents, for example to 
// aviod circular causation.  In the results, if the value of a variable 
// does not matter, 
// returns work, or GAVE_UP if we ran out of time.
int64 FindExplanationsForResult (Model *m, const Tuple & s, 
				 vector<pair<Rule *, Substitution> > *results,
				 set<Component *> * excluded_dependents,
				 int64 max_work); 

// Makes sure all of the propositions in the encoding of this rule
// are explained in the model.
void ExplainEncoding(Rule *r);


#endif
