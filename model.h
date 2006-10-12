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
  
  // Changes are tracked for the purpose of rolling them back when something
  // doesn't make the model better.
  // These are the types of changes.
  enum Action {
    INVALID_ACTION,
    ADD_COMPONENT,    // add a component to the model
    REMOVE_COMPONENT, // remove a component from the model
    CHANGE_STRENGTH,  // change the strength of a rule
    CHANGE_DELAY,     // change the delay on a rule
    NUM_ACTIONS,
  };
  struct Change{
    Action action_;
    // The component which was removed, if it was REMOVE_COMPONENT
    // we own the object at the end of this pointer.
    ComponentEssentials * component_essentials_; 
    int component_id_;
    EncodedNumber old_val_; // for change_strength or change_delay
    EncodedNumber old_val2_; // for change_strength
    string ToString() const; // just for debugging
    Change();
    ~Change();
  };
  // We keep a vector of changes as a history, and a checkpoint is simply
  // an index into that vector.  To roll back, just pop until that point.
  typedef uint Checkpoint;

  // construction/destruction

  Model();
  ~Model();
  
  // We need a fingerprint function on rules so that we can avoid
  // creating duplicate rules.
  static uint64 RuleFingerprint(RuleType type,
				const vector<Tuple> & precondition,
				const vector<Tuple> & result,
				const vector<Tuple> & target_precondition);
  
  // We can look up a component by id.
  Component * GetComponent(int id) const;
  template <class C> C * GetComponent(int id) const{
    return dynamic_cast<C *>(GetComponent(id));
  }

  // Returns the representation of a tuple (a clause in a rule).
  // Used in determining the encoding of rules.
  vector<Tuple> ComputeTupleEncoding(const Tuple &s, int name);
  // A global sanity check that the Dependents(), Codependents(), Purposes(),
  // and Copurposes() functions are returning what they should
  void CheckConnections();
  // A global check that we have been tracking the likelihood of the model
  // properly.
  void CheckLikelihood();
  
  // Finds all satisfactions of preconditions that involve a given true 
  // proposition.
  // returns work, or -1 if we run out of time.
  // In the results vector, it puts triples of precondition, number of
  // satisfactions, and actual satisfactions.  Some or all of the actual 
  // satisfactions may be omitted based on the settings of the last two 
  // parameters.
  // TODO, make actual_work a parameter, for uniformity sake.
  int64 FindSatisfactionsForProposition
    ( const Tuple & s, 
      vector<pair<Precondition *, pair<uint64, vector<Substitution> > > > 
      *results,
      int64 max_work,
      bool return_subs_for_negative_rules,
      bool return_subs_for_all_rules);
  
  // This is for editing and inspecting the model specification.
  // Tuples can be forbidden or required.  Forbidden tuples can include
  // variable(0), multiple instances of which need not match the same literal.
  // If a tuple is declared both forbidden and required, it is required and
  // not forbidden.  
  bool IsRequired(const Tuple & s);
  bool IsForbidden(const Tuple & s);
  void MakeRequired(const Tuple & s);
  void MakeNotRequired(const Tuple & s);
  void MakeForbidden(const Tuple & s);
  void MakeNotForbidden(const Tuple & s);
  
  // The specification can be read from a file.  A line in the file like
  // [ foo goo *moo ]
  // means that the tuple [ foo goo moo ] is required, and all other 
  // three-term tuples starting with "foo goo" are forbidden.
  void ReadSpec(istream * input); // do this first.
  // Does the model comply with the specification.
  bool Legal();

  // Makes this tuple given.  Creates a true proposition if one doesn't
  // exist, and flips the given bit to true.  
  void MakeGiven(const Tuple & s);

  // manipulation:

  // Assigns a fresh new ID to a component. 
  void AssignNewID(Component * component);
  void AssignSpecificID(Component * component, int id);
  // When a component is deleted, removes it from the id_to_component_ map.
  void ReleaseID(int id);

  // In some parts of the model, we need to encode terms, where context doesn't
  // help us.  This is accounted for by a global arbitrary term encoder, which
  // accounts for the entropy of encoding a sequence of terms, using the 
  // frequencies of the terms to reduce complexity, but not their order. 
  // The following functions are called to add and remove a term from that 
  // encoder, and to update the global likelihood.
  void AddArbitraryTerm(int w);
  void SubtractArbitraryTerm(int w);

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
  void L1_InsertIntoClauseToPreconditionMap(Precondition *p);
  void L1_RemoveFromClauseToPreconditionMap(Precondition *p);


  // data
  int next_id_;
  map<int, Component *> id_to_component_;
  TupleIndex tuple_index_; // stores pointers to Firings
  map<const Tuple *, TrueTuple *> index_to_true_proposition_;
  set<Component *> times_dirty_; //components whose times need fixing
  set<Component *> never_happen_; // components which never happen
  set<Component *> required_never_happen_; // required and never happen
  hash_map<uint64, set<pair<Precondition *, int> > > clause_to_precondition_;
  hash_map<uint64, set<pair<Rule *, int> > > clause_to_result_;
  hash_map<uint64, Precondition *> precondition_index_;
  map<uint64, Rule *> rule_index_;
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
