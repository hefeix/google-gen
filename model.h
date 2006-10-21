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



#ifndef _MODEL_H_
#define _MODEL_H_

#include "util.h"
#include "tuple.h"
#include "numbers.h"
#include "tupleindex.h"
#include "lexicon.h"
#include "record.h"
#include "component.h"
#include <set>
#include <map>

typedef uint Checkpoint;

class Model {
 public:
  // ----- LAYER 3 FUNCTIONS -----

  Model();
  ~Model();


  // ----- LAYER 2 FUNCTIONS -----

  // Add a requirement to the problem specification.
  TrueTuple * AddRequirementToSpec(Tuple *t);

  // Add a prohibition to the problem specification.  The first parameter
  // is a wildcard tuple to be forbidden, and the second is a list of
  // constant tuple exceptions.
  Prohibition * AddProhibitionToSpec(Tuple *prohibited,
                                     vector<Tuple> exceptions);

  // The specification can be read from a file.  A line in the file like
  // [ foo goo *moo ]
  // means that the tuple [ foo goo moo ] is required, and all other
  // three-term tuples starting with "foo goo" are forbidden.
  void ReadSpec(istream * input); // do this first.

  


  // ----- CONST FUNCTIONS -----

  // We can look up a component by id.
  Component * GetComponent(int id) const;
  template <class C> C * GetComponent(int id) const{
    return dynamic_cast<C *>(GetComponent(id));
  }

  // Checks that the ln_likelihood of the model is correctly the sum
  // of the ln_likelihood of all of the components plus the arbitrary term
  // naming costs.
  void CheckLikelihood() const;
  // Checks that all of the Layer 2 requirements are met.
  void VerifyLayer2() const;

  // Finds all satisfactions of preconditions that involve a given tuple.
  // If the tuple is not in the model, it pretends that it is.
  // returns work, or -1 if we run out of time.
  // In the results vector, it puts triples of precondition, number of
  // satisfactions, and actual satisfactions.  Some or all of the actual
  // satisfactions may be omitted based on the settings of the last two
  // parameters.
  // TODO, make actual_work a parameter, for uniformity sake.
  int64 FindSatisfactionsForTuple
    ( const Tuple & s,
      vector<pair<Precondition *, pair<uint64, vector<Substitution> > > >
      *results,
      int64 max_work,
      bool return_subs_for_negative_rules,
      bool return_subs_for_all_rules) const;

  // Do any prohibitions forbid this tuple
  bool IsForbidden(const Tuple & t) const;

  // Does the model comply with layer 3 requirements
  bool IsLayer3() const;


  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

    // Assigns a fresh new ID to a component. 
  void L1_AssignNewID(Component * component);
  // When a component is deleted, removes it from the id_to_component_ map.
  void L1_ReleaseID(int id);


  // manipulation:

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

  // Finds or adds a Precondition
  Precondition * GetAddPrecondition(const vector<Tuple> & tuples);
  
  // Finds a TrueTuple
  TrueTuple * FindTrueTuple(const Tuple & s);
  // Finds or adds a TrueTuple
  TrueTuple * GetAddTrueTuple(const Tuple & s);

  // Records a change to the model in the history.
  void RecordChange(Change * change);
  // Undoes a recorded change.
  void UndoChange(const Change & change);
  // These are methods for recording particular types of changes to the model.
  void RecordAddComponent(const Component * c);
  void RecordRemoveComponent(const Component * c);
  void RecordChangeStrength(const Rule * r, EncodedNumber old_strength,
			    EncodedNumber old_strength2);
  void RecordChangeDelay(const Rule * r, EncodedNumber old_delay);
  // Creates a checkpoint object that can be used in the future to roll back 
  // the model to the current state. 
  inline Checkpoint MakeCheckpoint() { return history_.size();}
  // Rolls back the model to the state at which the checkpoint was created.
  void Rollback(Checkpoint checkpoint);
  // Adds a component to the model, given a record that was stored by a 
  // ComponentEssentials object. 
  Component * AddComponentFromRecord(Record r);

  // Sort the components in the set into an order in which they could be
  // inserted legally into the model, or could be legally removed in
  // the opposite order.
  vector<Component *> 
    SortIntoLegalInsertionOrder(const set<Component *> & to_insert);
  // A recursive subroutine of the above function.  
  // Pre and post-condition: to_insert and result are disjoint.
  // Adds the component "which" and all recursive necessary codependents in the 
  // to_insert set to the result vector and removes them from the to_insert 
  // set. 
  void SortIntoLegalInsertionOrderInternal(set<Component *>* to_insert,
					   vector<Component*>*result,
					   Component * which);
  
  // removes a component, and if just_this is set to false, all of the
  // components that are left purposeless by removing it.
  void KillComponent(Component *to_kill, bool just_this = false);
  // Makes all of the times correct and sets the dirty bits to false
  // Some of the times may end up as NEVER
  void FixTimes();
  // Deletes all of the components whose times are NEVER
  void DeleteNeverHappeningComponents();

  // misc.
  // Inserts or retrieces a creative rule with no preconditions and one 
  // postcondition full of different variables.  Such rules can explain
  // anything.
  Rule * GetAddNaiveRule(int length); // {} -> { [ $0 $1 ... $(length-1)] }

  // I/O
  // A bar of links to the files in the HTML display.
  string LinkBar();
  // Writes the model to html in the given relative directory.
  void ToHTML(string dirname);
  // A record full of statistics about the model as a whole
  Record ModelInfo();
  // Stores the model in machine readable form.
  void Store(ostream * output);
  void StoreToFile(string filename);
  // Loads a model
  void Load(istream * input);
  




  
  // human interaction
  void Shell(istream * input);


  // Simple L1 modifiers
  void A1_InsertIntoWildcardTupleToPreconditionMap
    (Tuple t, Precondition *p, int position);
  void A1_RemoveFromWildcardTupleToPreconditionMap
    (Tuple t, Precondition *p, int position);
  void A1_InsertIntoWildcardTupleToResultMap
    (Tuple t, Rule *r, int position);
  void A1_RemoveFromWildcardTupleToResultMap
    (Tuple t, Rule *r, int position);



  // data
  int next_id_;
  map<int, Component *> id_to_component_;
  TupleIndex tuple_index_; // stores pointers to Firings
  map<Tuple, TrueTuple *> tuple_to_true_tuple_;
  set<Component *> times_dirty_; //components whose times need fixing
  set<Component *> never_happen_; // components which never happen
  set<TrueTuple *> required_never_happen_; // required and never happen
  // maps clauses found in preconditions
  hash_map<Tuple, set<pair<Precondition *, int> > > clause_to_precondition_;
  // same thing for the results of rules.  
  hash_map<Tuple, set<pair<Rule *, int> > > clause_to_result_;
  hash_map<vector<Tuple>, Precondition *> precondition_index_;
  // maps the prohibited tuple of a prohibition to the prohibition.  
  map<Tuple, set<Prohibition *> > prohibition_index_;
  vector<Change *> history_;
  map<int, int> arbitrary_term_counts_;
  int total_arbitrary_terms_;
  double arbitrary_term_ln_likelihood_; // superfluous
  double ln_likelihood_;

  // the spec
  map<uint64, Tuple> required_;
  map<uint64, Tuple> forbidden_;
  set<TrueTuple *> present_forbidden_;
  set<uint64> absent_required_;  

};

// utility for turning vectors of pointers to components to vectors of stable 
// pointers.
template<class C> vector<Model::StablePtr<C> > 
ToStablePtrVector(const vector<C*> &v) {
  return vector<Model::StablePtr<C> >(v.begin(), v.end());
}

#endif
