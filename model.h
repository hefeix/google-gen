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

#ifndef _MODEL_H_
#define _MODEL_H_

#include "util.h"
#include "tuple.h"
#include "numbers.h"
#include "tupleindex.h"
#include "lexicon.h"
#include "record.h"
#include "component.h"
#include "changelist.h"
#include <set>
#include <map>

#define GAVE_UP (-1)

struct SearchNode;
class TupleIndex;

// Used in combining rules
// GEORGES - COMMENT
struct SubRuleInfo {
  Rule *        rule_;
  // This is a variable to variable sub to get to the common canonicalized form
  Substitution  sub_;
  bool          postcondition_;

  SubRuleInfo() {
    rule_ = NULL;
    postcondition_ = false;
  }

  bool operator<(const SubRuleInfo& s) const {
    // Ironically pointers are more stable than IDs
    if (rule_ < s.rule_) return true;
    if (rule_ == s.rule_)
      if (sub_ < s.sub_) return true;
    if ( (rule_ == s.rule_ ) && ( sub_ == s.sub_ ) )
      if (postcondition_ < s.postcondition_) return true;
    return false;
  }

  string ToString() const {
    stringstream ret;
    ret << "RuleID:" << rule_->GetID() << endl;
    ret << "RulePtr:" << (uint64) rule_ << " ";
    ret << "Sub:" << sub_.ToString() << endl;
    if (postcondition_) 
      ret << "Postcondition" << endl;
    return ret.str();
  }
};
inline ostream& operator<<(ostream& o, const SubRuleInfo& sri) {
  return (o << sri.ToString());
}

class Model {
 public:
  friend class Precondition;
  friend class Satisfaction; 
  friend class RuleSat;
  friend class Firing; 
  friend class TrueTuple;
  friend class Rule;
  friend class Component;
  friend class Prohibition;
  friend class SearchNode;

  // ----- LAYER 3 FUNCTIONS -----

  Model();
  ~Model();


  // ----- LAYER 2 FUNCTIONS -----

  // Finds or adds a TrueTuple
  TrueTuple * GetAddTrueTuple(const Tuple & s);

  // Add a requirement to the problem specification.
  TrueTuple * AddRequirementToSpec(Tuple t);

  // Add a prohibition to the problem specification.  The first parameter
  // is a wildcard tuple to be forbidden, and the second is a list of
  // constant tuple exceptions.
  Prohibition * AddProhibitionToSpec(Tuple prohibited,
                                     vector<Tuple> exceptions);

  // The specification can be read from a file.  A line in the file like
  // [ foo goo *moo ]
  // means that the tuple [ foo goo moo ] is required, and all other
  // three-term tuples starting with "foo goo" are forbidden.
  void ReadSpec(istream * input); // do this first.

  // This is how new rules are created.
  Rule * MakeNewRule(vector<Tuple> precondition, EncodedNumber delay,
		     RuleType type, Rule * target_rule,
		     vector<Tuple> result, 
		     EncodedNumber strength, EncodedNumber strength2){
    return
      new Rule(L1_GetAddPrecondition(precondition),
	       delay, type, target_rule, result, strength, strength2);
  }
  
  // Inserts or retrieves a creative rule with no preconditions and one 
  // postcondition full of different variables.  Such rules can explain
  // anything.
  Rule * GetAddUniversalRule(uint length); // {} -> { [ $0 $1 ... $(length-1)] }

  // Makes all of the times correct and sets the dirty bits to false
  // Some of the times may end up as NEVER
  bool FixTimes();

  // Deletes all of the components whose times are NEVER
  // Precondition: times are all clean.
  void DeleteNeverHappeningComponents();
  
  // used when loading models.  If another component already is using the new
  // id, it swaps the two ids.  
  void ChangeID(Component *c, int new_id);

  // ----- CONST FUNCTIONS -----

  // We can look up a component by id.
  Component * GetComponent(int id) const;
  template <class C> C * GetComponent(int id) const{
    return dynamic_cast<C *>(GetComponent(id));
  }

  // Get the number of true tuples
  inline uint GetNumTrueTuples() {
    return tuple_to_true_tuple_.size();
  }
  inline const map<Tuple, TrueTuple*>& GetTupleToTrueTuple() {
    return tuple_to_true_tuple_;
  }

  inline const map<Pattern, set<SubRuleInfo> >& GetSubrulePatternToRule() {
    return subrule_pattern_to_rule_;
  }

  // Checks that the ln_likelihood of the model is correctly the sum
  // of the ln_likelihood of all of the components plus the arbitrary term
  // naming costs.
  void VerifyLikelihood() const;
  
  // Checks that the TemporalDependents() and TemporalCodependents() functions 
  // of components are converses and that Purposes() and Copurposes() are
  // converses.
  void VerifyLinkBidirectionality() const;

  // Checks that all of the indices are up to date. 
  void VerifyIndices() const;
  
  // Checks that all of the Layer 2 requirements are met.
  void VerifyLayer2() const;

  // Finds possible pairs of rules and substitutions such that the 
  // preconditios are satisfied, and one of the results is the given tuple.
  // You can exclude some TrueTuples as dependents, for example to 
  // aviod circular causation.  In the results, if the value of a variable 
  // does not matter, sets it to the word "whatever".   TODO: wtf?
  bool FindExplanationsForResult (const Tuple & t, 
				  vector<pair<Rule *, Substitution> > *results,
				  const set<Component *> * excluded_dependents,
				  int64 *max_work_now); 
  
  
  // Finds a TrueTuple
  TrueTuple * FindTrueTuple(const Tuple & s) const;

  // Finds a positive rule
  Rule * FindPositiveRule(vector<Tuple> precondition, vector<Tuple> result)
    const;
  // Finds a negative rule
  Rule * FindNegativeRule(vector<Tuple> precondition, Rule * target_rule) const;

  // Do any prohibitions forbid this tuple
  bool IsForbidden(const Tuple & t) const;

  // Does the model comply with layer 3 requirements
  bool IsLayer3() const;

  // FixTime may be able to make the model Layer 3
  bool MayBeTimeFixable() const;

  double GetLnLikelihood() const { return ln_likelihood_;}
  double GetArbitraryTermLnLikelihood() const 
  { return arbitrary_term_ln_likelihood_;}

  double GetSearchWork() const { return search_work_;}
  double GetUtility() const { return ln_likelihood_ 
      - search_work_ * work_penalty_; } 
  
  Precondition * FindPrecondition(const vector<Tuple> & tuples) const;

  set<Rule *> GetAllRules() const;

  TupleIndex * GetTupleIndex() { return &tuple_index_;}
  const TupleIndex * GetTupleIndex() const { return &tuple_index_;}

  Changelist * GetChangelist() { return &changelist_;}

  const set<TrueTuple *> & GetRequiredNeverHappen() 
    { return required_never_happen_;}
  const set<Component *> & GetNeverHappen() 
    { return never_happen_;}
  const set<Component *> & GetTimesDirty() const { return times_dirty_;}
  const set<Component *> & GetComponentsOfType(ComponentType t) const{
    return components_by_type_[t];
  }

  int ArbitraryTermCount(int term) const;

  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

    // Assigns a fresh new ID to a component. 
  void L1_AssignNewID(Component * component);
  // When a component is deleted, removes it from the id_to_component_ map.
  void L1_ReleaseID(int id);


  // In some parts of the model, we need to encode terms, where context doesn't
  // help us.  This is accounted for by a global arbitrary term encoder, which
  // accounts for the entropy of encoding a sequence of terms, using the 
  // frequencies of the terms to reduce complexity, but not their order. 
  // The following functions are called to add and remove a term from that 
  // encoder, and to update the global likelihood.
  void L1_AddArbitraryTerm(int w);
  void L1_SubtractArbitraryTerm(int w);

  // Call these functions after adding(removing) a tuple to(from) the tuple 
  // index to keep the search trees for the preconditions updated.
  void L1_UpdateSearchTreesAfterAddTuple(Tuple t);
  void L1_UpdateSearchTreesAfterRemoveTuple(Tuple t);

  // Finds or adds a Precondition
  Precondition * L1_GetAddPrecondition(const vector<Tuple> & tuples);

  // misc.
  string FindName(string base);

  // I/O
  // writes the model to a file (including the spec)
  void Store(string filename) const;

  // reads the model from a file (including ths spec)
  void Load(string filename);

  // A bar of links to the files in the HTML display.
  string LinkBar() const;
  string DLinkBar() const;

  // Writes the model to html in the given relative directory.
  void ToHTML(string dirname) const;

  // Writes all components of one type as HTML to a string
  void ToHTMLByComponentType(stringstream& out, 
			     const set<ComponentType>& ct);

  // A record full of statistics about the model as a whole
  Record ModelInfo() const;
  
  // human interaction
  void Shell(istream * input);

 private:    

  // Simple A1 modifiers
  void A1_SetLnLikelihood(double new_val);
  void A1_InsertIntoIDToComponent(int id, Component *c);
  void A1_RemoveFromIDToComponent(int id);
  void A1_InsertIntoComponentsByType(Component *c);
  void A1_RemoveFromComponentsByType(Component *c);
  void A1_InsertIntoTupleToTrueTuple(Tuple t, TrueTuple *tt);
  void A1_RemoveFromTupleToTrueTuple(Tuple t);
  void A1_InsertIntoTimesDirty(Component *c);
  void A1_RemoveFromTimesDirty(Component *c);
  void A1_InsertIntoNeverHappen(Component *c);
  void A1_RemoveFromNeverHappen(Component *c);
  void A1_InsertIntoRequiredNeverHappen(TrueTuple *c);
  void A1_RemoveFromRequiredNeverHappen(TrueTuple *c);  
  void A1_InsertIntoWildcardTupleToPrecondition
    (Tuple t, Precondition *p, int position);
  void A1_RemoveFromWildcardTupleToPrecondition
    (Tuple t, Precondition *p, int position);
  void A1_InsertIntoWildcardTupleToResult(Tuple t, Rule *r, int position);
  void A1_RemoveFromWildcardTupleToResult(Tuple t, Rule *r, int position);
  void A1_InsertIntoSubrulePatternToRule(Pattern p, SubRuleInfo s);
  void A1_RemoveFromSubrulePatternToRule(Pattern p, SubRuleInfo s);
  void A1_InsertIntoPreconditionIndex(const Pattern &pat, Precondition *p);
  void A1_RemoveFromPreconditionIndex(const Pattern &pat);
  void A1_InsertIntoProhibitionIndex(Tuple t, Prohibition *p);
  void A1_RemoveFromProhibitionIndex(Tuple t, Prohibition *p);
  void A1_InsertIntoSpecRequirements(TrueTuple *t);
  void A1_RemoveFromSpecRequirements(TrueTuple *t);
  void A1_InsertIntoSpecProhibitions(Prohibition *p);
  void A1_RemoveFromSpecProhibitions(Prohibition *p);
  void A1_AddToArbitraryTermCounts(int t, int delta);
  void A1_AddToTotalArbitraryTerms(int delta);
  void A1_AddToArbitraryTermLnLikelihood(double delta);
  void A1_AddToLnLikelihood(double delta);
  void A1_AddToSearchWork(int64 delta);
  void A1_InsertIntoViolatedProhibitions(Prohibition *p);
  void A1_RemoveFromViolatedProhibitions(Prohibition *p);
  void A1_IncrementNextID();

  // data
  // When components are added to the model, they get sequential ids.
  int next_id_;
  // Maps id to the component with that ID (only com
  map<int, Component *> id_to_component_;
  set<Component *> components_by_type_[NUM_COMPONENT_TYPES];
  TupleIndex tuple_index_; // stores pointers to Firings
  map<Tuple, TrueTuple *> tuple_to_true_tuple_;
  set<Component *> times_dirty_; //components whose times need fixing
  set<Component *> never_happen_; // components which never happen
  set<TrueTuple *> required_never_happen_; // required and never happen
  // We index the tuples in preconditions by turning their variables to
  // wildcards.  The second element in the pair is the position of the tuple
  // in the pattern.
  map<Tuple, set<pair<Precondition *, int> > >  wildcard_tuple_to_precondition_;
  // same thing for the results of rules.  
  map<Tuple, set<pair<Rule *, int> > > wildcard_tuple_to_result_;
  // Preconditions indexed by their pattern.
  map<Pattern, Precondition *> precondition_index_;
  // maps the prohibited tuple of a prohibition to the prohibition.  
  map<Tuple, set<Prohibition *> > prohibition_index_;
  // The problem specification.  We don't really need to have this around, 
  // but let's keep it around in case we need it later.
  set<TrueTuple *> spec_requirements_;
  set<Prohibition *> spec_prohibitions_;

  // Index rules by subpatterns
  map<Pattern, set<SubRuleInfo> > subrule_pattern_to_rule_;
  
  // Which prohibitions are currently violated
  set<Prohibition *> violated_prohibitions_;
  
  map<int, int> arbitrary_term_counts_;
  int total_arbitrary_terms_;
  double arbitrary_term_ln_likelihood_; // superfluous

  map<Tuple, set<SearchNode *> > wildcard_tuple_to_search_node_;

  // Total of search_work_ for all preconditions.
  uint64 search_work_; 
  // We prefer models that cost us less work in searching for satisfactions of 
  // preconditions, since they are quicker to reason about.  The number of 
  // units of search work is multiplied by this number and subtracted from the
  // ln_likelihood_ of the model (in nats).  Guess: make this number about 
  // 1/1000
  double work_penalty_;
  
  double ln_likelihood_;

  Changelist changelist_;
  bool old_style_display_;

  vector<string> words_; // This is for randomly generating words
};

#endif
