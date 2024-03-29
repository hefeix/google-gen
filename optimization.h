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

#ifndef _OPTIMIZATION_H_
#define _OPTIMIZATION_H_

#include "model.h"

class Optimizer;

enum Tactic {
  NEW_RULE,
  NEW_MANY_EXAMPLES_RULE,
  SPECIFY_ONE,
  GENERALIZE_ONE,
};

// This is an object that can be created, which will set a checkpoint upon
// creation, and automatically roll back the model if it doesn't like the
// model when it goes out of scope.  
struct OptimizationCheckpoint { 
  Model * model_;
  Optimizer * optimizer_;
  Checkpoint cp_;
  LL old_utility_;
  // should we make sure all of the times are correct in the model
  // before deciding whether to roll back 
  bool fix_times_;  
  bool logging_;
  OptimizationCheckpoint(Optimizer *optimier, bool fix_times);
  ~OptimizationCheckpoint();
  // Should we keep the changes
  bool KeepChanges();
  // The increase in model utility since creation.
  LL Gain();
  private:
  // Do we think that the model has gotten better since creation.
  bool Better();
};

struct Optimizer{
  Optimizer(Model *m);
 
  // OPTIMIZATION functions. 
  // There are no built-in rollback mechanisms.  Wrap them in an automated 
  // roll-back if you want to.
  
  // Removes a firing and might remove the rule.  
  void TryRemoveFiring(Firing *f);
  
  void PushTimesThroughRuleSats(TrueTuple *tt);

  // Propagates the time change after we change the delay of a rule
  // through the true tuples it causes and the feature rule sats in which
  // they participate.
  // This is to avoid fixing all of the times in the model.
  void PushTimesAfterChangeDelay(Rule *rule);

  // Adds a bunch of firings at once and removes alternate explanations.
  void TryAddFirings(Rule * r, const vector<Substitution> & sub, 
		     int max_recursion);
  
  // Given a creative rule, tries creating rules where the unbound RHS variables
  // in the original rule are replaced by constants.
  //void TrySpecifyCreativeRule(Rule * r);
  
  // TODO, document this
  void TryAddPositiveRule(const vector<Tuple> & preconditions, 
			  const vector<Tuple> & result,
			  int max_recursion, 
			  string comments,
			  bool add_to_recently_checked);
  
  void TryRuleVariations(const Pattern & preconditions, 
			 const Pattern & result, 
			 int max_recursion);
  
  Rule * TryMakeFunctionalFeatureRule(Rule *r);
  // HERE
  void MakeFeatureRuleSatsHappenInTime(const set<RuleSat *> rule_sats);
  void OptimizeRuleStrengths(set<Rule *> rules);
  
  // Combining rules
  bool CombineRules(int time_limit, string * comments);
  bool MaybeCombineRules(string * comments);
  void TryCombineRules(Pattern lhs,
		       const vector<SubRuleInfo>& info,
		       string * comments);

  // finds a candidate rule unless it runs out of time.  
  // time_limit is in seconds
  bool FindRandomCandidateRule(CandidateRule *ret, 
			       Tactic tactic, int time_limit, string *comments);
  bool MaybeFindRandomCandidateRule(CandidateRule * ret, Tactic tactic, 
				    string *comments);
  
  struct PatternBuilder {
    Pattern pattern_;
    Time    target_time_;
    vector<Substitution> subs_;
    Optimizer * optimizer_;
    map<set<int>, int> anchor_sets_tried_;
    PatternBuilder(Optimizer * opt) {
      optimizer_ = opt;
    }
    PatternBuilder(Optimizer * opt, Pattern pattern,
		   const vector<Substitution>& subs) {
      optimizer_ = opt;
      pattern_ = pattern;
      subs_ = subs;
    }

    bool TryInitializeFromSurprisingTuple();

    bool ExpandFully(uint size);
    bool TryExpandOnce();

    void CollapseEquivalentVariables();
    void CollapseConstantVariables();
    string ToString();
  };

  bool FindSampling(const Pattern & p, SamplingInfo * result, 
		    int64 max_work,
		    vector<Substitution> * subs,
		    uint64 * estimated_num_results,
		    uint64 * actual_num_results,
		    set<uint> * bad_clauses, // don't sample these
		    SamplingInfo *hint);

  // How useful is it to generate this another way
  LL GuessBenefit(const TrueTuple * tp);
  Tuple  GetRandomSurprisingTuple();

  int64 StandardMaxWork();
  int64 ConstantExpectationMaxWork();
  bool VetteCandidateRule(CandidateRule r, 
			  CandidateRule * simplified_rule, 
			  int64 max_work, string *comments);
  
  bool MaybeFindRandomNewRule(CandidateRule * ret, string *comments);
  bool MaybeFindManyExamplesRule(CandidateRule * ret, string *comments);
  bool MaybeFindRandomVariantRule(CandidateRule *ret, Tactic tactic, 
				  string *comments);
  
  // OPTIMIZATION STEPS
  //void OptimizeRound(Model *m);
  
  TrueTuple * GetRandomTrueTuple();

  ComputationResult DependsOn(Component * dependent, Component * dependee, 
			    int64 max_work);

  // determines whether deleting this component will (recursively) remove
  // any of the required true propositions.  
  ComputationResult IsEssential(Component *c, int max_work, 
				int * actual_work);
  
  // Would component c go away, directly or indirectly, if we removed 
  // Component d?  The function doesn't recursively inspect, so sometimes
  // it is not sure whether there is an indirect dependency and it can
  // return RESULT_MAYBE.
  ComputationResult RequiresCodependent(Component * dependent, 
					Component * codependent);
  
  void OptimizeStrength(Rule *r);
  
  // Takes an unexplained TrueTuple, and finds the most probable firing 
  // to add to the model that will explain that TrueTuple.  
  // If an "excluded" set is given, we avoid adding firings which depend on
  // the excluded TrueTuples.
  void Explain(TrueTuple * tt,
	       const set<Component *> * excluded,
	       bool fix_times); // satisfies in the best way it can
  // Explain all of the required propositions.
  
  // I don't think we need this now that we have FixTimesFixCircularDependencies.
  //void FulfillRequirements(Model *m); // call on an empty model
  
  // Fixes the times.  Adds explanations for required things that never happen,
  // and deletes all remaining components whose time is NEVER
  // returns false if it can't fix things in 10 seconds
  bool FixTimesFixCircularDependencies(int time_limit = 200);
  
  // data:
  Model *model_;
  // maps a condidate rule to the utility of the model the last time
  // we tried to add the rule.
  map<CandidateRule, LL> recently_checked_;
};
// Makes sure all of the propositions in the encoding of this rule
// are explained in the model.
// void ExplainEncoding(Rule *r);


#endif
