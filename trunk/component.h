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


#ifndef _COMPONENT_H_
#define _COMPONENT_H_

// A note on ensuring model consistency:
// We classify functions that modify the model into layers.  Each layer 
// must also respect the restrictions of lower layers.  
//
// Layer 0: Anything goes.
//
// Layer 1: The effects can be completely rolled back using the changelist.
//          Layer 1 is genreally for simple modification functions.
//
// 
//
// Layer 2: The following local consistency criteria are maintained:
//           a. Component and model likelihoods are correct.
//           b. Links are bidirectional ( and the other side exists )
//           c. Component times are correct, or the time_dirty_ flag is on.
//           d. All of the indices kept by the model are up to date.
//               TODO( include list of indices )
//           e. Components that need a purpose have a purpose.
//           f. All satisfactions of feature rules have their rulesats
//              represented explicitly.
//           g. The preconditions' satisfaction counts are correct.
//
// Layer 3: Global consistency:
//           a. The times are all clean.
//           b. All required tuples happen, and no forbidden ones do.
//           c. No times are set to NEVER.
//
// About the Component subclass constructors:  All of them obey all criteria
// for layer 2, except that components can exist without a purpose.  
// Also, objects may not be created on the stack, as they push a change on the
// changelist which calls delete on a rollback. 
//  TODO: make the constructors private and write factory methods which
// specify layers.

#include "util.h"
#include "record.h"
#include "tuple.h"
#include "numbers.h"

class Model;
class Chooser;
class Component; //  a basic part of the model.  There are several subtypes.

// These are the types of components
class Precondition;  // The preconditions of a rule
class Rule;
class Satisfaction; // An instance of a Precondition being satisfied.
class RuleSat;      // An instance of a rule being satisfied.
class Firing;       // An instance of a rule firing.
class TrueTuple;

class SearchTree;
class SearchNode;

// Something else entirely
class Prohibition;

enum ComponentType {
  PRECONDITION,
  RULE,
  SATISFACTION,
  RULESAT,
  FIRING,
  TRUETUPLE,
  NUM_COMPONENT_TYPES,
};

// Converting back and forthe between component types and their names.
ComponentType StringToComponentType(const string & s); 
string ComponentTypeToString(ComponentType t);

// These are the types of rules.
enum RuleType {
  INVALID_RULE, // not used
  SIMPLE_RULE,  // no new variables on RHS.  Can fire only once per rule_sat
  FEATURE_RULE, // Conditions another rule.
  CREATIVE_RULE, // New variables on RHS.  Can have multiple firings.
  NUM_RULE_TYPES,
};
RuleType StringToRuleType(const string & s);
string RuleTypeToString(RuleType t);

// This is the current state of a rulesat.  
// We keep the state in line with the data that is tracked at the rule,
// in order to avoid bugs.  
enum RuleSatState{
  RS_FEATURE, // it's a feature rule
  RS_NO_DECISION, // no decision is required - simple rule and something
          // already causes the result before this rulesat (commented out now)
  RS_NO_FIRING, // no firings
  RS_FIRST_FIRING, // at least one firing
};

class Component{
 public:
  friend class Model;

  // ----- LAYER 2 FUNCTIONS -----

  // This function makes the component not exist.  It also first erases its
  // StructuralDependents, and its Copurposes that no loner have a purpose.  
  // You can call Erase() on anything but Satisfaction and RuleSat objects.
  // Those can't be removed in a Layer 2 manner (since all rule_sats of 
  // a feature rule must be represented), so call L1_Erase.
  void Erase(); // but don't delete.
  // The time_ field is set lazily to save time.  Sometimes it is locally 
  // correct (clean) and sometimes it might be localy incorrect (dirty).  
  // Clean does not mean globally correct, and calling ComputeSetTime()
  // on every component does not make all the times clean or correct.
  // Call FixTimes() on the model to make all of the times clean and correct.
  // This function computes and sets the time for this component, and makes
  // the dirty bit for this component false.  It may make the dirty bits 
  // for other components true.
  // return true if the time changed.
  bool ComputeSetTime(); 
  // Computes and sets the ln_likelihood_ for this component, and adjusts
  // the total ln likelihood of the model. 
  void F2_ComputeSetLnLikelihood();
  // If the time of a component changes, we call this function, which 
  // adjusts the ln_likelihood_ of this and other components if necessary.
  virtual void F2_AdjustLnLikelihoodForNewTime();
  void AddComments(string new_comments);


  // ----- CONST FUNCTIONS -----

  Model * GetModel() const { return model_;}

  virtual ComponentType Type() const { return NUM_COMPONENT_TYPES; }
  string TypeName() const;

  void VerifyLayer2() const;
  void VerifyLnLikelihood() const;
  void ProbabilisticallyVerifyLnLikelihood() const;
  virtual void VerifyLayer2Subclass() const;
  
  // A link to this component in the HTML viewer.
  string HTMLLink(string text) const;
  
  // Key-Value pairs to be displayed in the HTML viewer. 
  Record RecordForDisplay(bool verbose) const;
  
  // Type-specific key-value pairs.  Called by RecordForDisplay()
  virtual Record RecordForDisplaySubclass(bool verbose) 
    const {CHECK(0); return Record(); }

  // Used when storing the model
  Record RecordForStorge() const;
  virtual Record RecordForStorageSubclass() const {CHECK(0); return Record(); }
  
  // Returns pointers to the components that structurally depend on this one
  // I.e. without this component, the others may not exist in a layer 2 model.
  virtual vector<Component *> StructuralDependents() const;

  // Returns pointers to the components that depend in some way on this 
  // component.  They may have other options for existing, so they don't 
  // necessarily disappear when this component does.
  virtual vector<Component *> TemporalDependents() const;
  
  // Components that this component depens on. 
  // The vector of vectors is to be interpreted as an AND of ORs.
  // This component needs at least one element of each vector to exist.
  // Sometimes, one of these vectors can be empty, but that means that 
  //   the component never happens.  It means that the model is invalid, but
  //   it can be an intermediate state in the search, and doesn't break the
  //   code.
  virtual vector<vector<Component *> > TemporalCodependents() const;
  
  // Some components have no reason to exist without other components.
  // These other componets are their "Purposes" for existing.  When the last 
  // one gets deleted, the component is automatically deleted.  Other 
  // components do not need a purpose to exist. (see NeedsPurpose()). 
  virtual vector<Component *> Purposes() const;
  
  // Components which have this component as a purpose.
  virtual vector<Component *> Copurposes() const; // inverse of above

  // Does this component need a purpose.
  virtual bool NeedsPurpose() const; // defaults to false unless overriden.
  virtual bool HasPurpose() const;  // Does it have a purpose.
  inline bool IsSuperfluous() const {return NeedsPurpose() && !HasPurpose();}  
  inline bool Exists() const { return exists_; }
  inline bool ReallyDead() const { return really_dead_; }

  // Computes the time_ of the component.  This is in general equal to the 
  // maximinimum time of its TemporalCodependents() (or NEVER if it is missing the
  // required codependents to exist), but some component types have an 
  // additional time delay. 
  // You can specify an excluded set of components.  The function pretends
  // that these components do not exist in the model. 
  Time ComputeTime(set<Component *> * excluded) const; 
  virtual inline bool HasTimeDelay() const{ return false;}
  // The time delay is the coordinate of the time that gets incremented by 1
  virtual inline EncodedNumber GetTimeDelay() const { return EncodedNumber();}
  
  // Computes the local contribution of this component to the ln likelihood 
  // of the model.
  virtual double LnLikelihood() const;
  // Some simple sanity checks on the connection structure.
  void CheckConnections() const; // checks that Co<X>() is the iverse of <X>()
  int GetID() const { return id_;}

  const Time & GetTime() const { return time_;}
  
  string GetComments() const { return comments_;}

  // ----- CONSTRUCTOR(S) -----

  // The constructor should leave the exists_ bit false, and the destructor
  // should CHECK that it is false.  The constructor and destructor should 
  // not touch the rest of the model.
 protected:
  Component(Model * model);
 public:
  virtual ~Component();

 protected:
  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  // Set the time to new_time.
  // If the make_dependents_dirty parameter is true, as it should usually be, 
  // the temporal dependents are checked and their dirty bits are set to true
  // if necessary.
  //
  // The indices are maintained.
  void L1_SetTimeMaintainIndices(Time new_time, bool make_dependents_dirty);

  // Erase any object, and recursively erase its structural dependents, and
  // anything that becomes purposeless. 
  // This function is layer 2, except on Satisfaction and feature RuleSat 
  // objects.
  void L1_Erase();
  // the subclass-specific parts of the erase function.
  virtual void L1_EraseSubclass() {CHECK(0); }

  // flips the dirty bit on, and inserts into global set of dirty components.
  void L1_MakeTimeDirty();
  // flips the dirty bit off, and removes from global set of dirty components.
  void L1_MakeTimeClean();

  // Adds to the ln_likelihood of this component, and adjusts the total ln 
  // likelihood of the model.
  void L1_AddToLnLikelihoodAndVerify(double delta);

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  // These things just modify the actual values.
  void A1_SetExists(bool val);
  void A1_SetReallyDead(bool val);
  void A1_SetTime(const Time & new_time);
  void A1_SetTimeDirty(bool val); // Use the L1 functions above instead!
  void A1_SetLnLikelihood(double new_ln_likelihood); 

  // ----- DATA -----

  // All data should only be touched by reversible functions.
  
  // If we delete a component and want to restore it on rollback, we want to
  // avoid invalidating pointers and storing the data elswhere.  Thus we just 
  // set the exists_ variable to false and pretend that it doesn't exist.
  bool exists_;

  // Worse than exists
  bool really_dead_;

  int id_;
  Model * model_;
  // contribution of this component to the ln_likelihood_ of the model
  double ln_likelihood_;
  Time time_;
  // time_dirty_ is set to true unless the time is set correctly based on the 
  // codependents of this component (though not necessarily globally).
  bool time_dirty_;

  string comments_;
};

// Here we have the six types of components.

// A Precondition represents the preconditions of a rule, which are a vector
// of tuples containing variables.
// The ln_likelihood_ aggregates the likelihood of all of its rules never
// firing.
class Precondition : public Component {
 public:
  friend class Rule; 
  friend class Satisfaction; 
  friend class RuleSat;
  friend class Firing; 
  friend class TrueTuple;
  friend class Component;
  friend class Model;
  friend class SearchNode;

  // ----- LAYER 2 FUNCTIONS ----- Precondition
  
  // TUPLE ENCODING STUFF
  // change encoding schemes.
  //void SwitchToTupleEncoding();
  //void SwitchToDirectEncoding();
  
  // ----- CONST FUNCTIONS ----- Precondition

  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  bool HasPurpose() const;
  vector<Component *> Purposes() const; // Rules
  vector<Component *> TemporalDependents() const; // Rules, Satisfactions
  vector<Component *> StructuralDependents() const;
  bool NeedsPurpose() const; // yes
  double LnLikelihood() const;
  // returns pointer to a satisfaction object if one exists.
  Satisfaction * FindSatisfaction(const Substitution &sub) const;
  void VerifyLayer2Subclass() const;
  // If there are multiple such rules, returns an arbitrary one.
  Rule * FindPositiveRule(const vector<Tuple> & result) const;
  set<Rule *> FindPositiveRules(const vector<Tuple> & result) const;
  Rule * FindFeatureRule(Rule * target_rule) const;
  int GetNumSatisfactions() const { return num_satisfactions_;}
  const Pattern & GetPattern() const { return pattern_;}
  double GetDirectPatternEncodingLnLikelihood() const {
    return direct_pattern_encoding_ln_likelihood_;
  }

  // TUPLE ENCODING STUFF
  // figures out what tuples cause the precondition under the tuple encoding.
  // set<Tuple> ComputeTupleCauses() const;

 private:
  // ----- CONSTRUCTOR(S) ----- Precondition

  Precondition(Model * model, const vector<Tuple> & tuples);

  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- Precondition

  // Given a precondition and a substitution that satisfies the precondition, 
  // creates a satisfaction object and links it to the precondition. 
  // If the satisfaction object already exists, it just returns it. 
  Satisfaction * L1_GetAddSatisfaction(const Substitution & sub);
  
  // Functions of the superclass Component()
  void L1_EraseSubclass();

  // TUPLE ENCODING STUFF
  //  These functions add and remove the global costs associated with the
  // different encoding schemes.   
  //void L1_MakeDirectlyEncoded();
  //void L1_MakeNotDirectlyEncoded();
  //void L1_MakeTupleEncoded();
  //void L1_MakeNotTupleEncoded();

  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- Precondition

  void A1_AddRule(Rule *r);
  void A1_RemoveRule(Rule *r);
  void A1_AddFeatureRule(Rule *r);
  void A1_RemoveFeatureRule(Rule *r);
  void A1_AddToFeatureRuleIndex(Rule * target_rule, Rule * feature_rule);
  void A1_RemoveFromFeatureRuleIndex(Rule * target_rule, Rule * feature_rule);
  void A1_AddToPositiveRuleIndex(const Pattern& result, Rule * rule);
  void A1_RemoveFromPositiveRuleIndex(const Pattern& result, Rule * rule);
  void A1_AddSatisfaction(Satisfaction * sat);
  void A1_RemoveSatisfaction(Satisfaction *sat);
  // Changes the total number of satisfactions (including ones not represented)
  void L1_AddToNumSatisfactions(int delta);

  // ----- DATA ----- Precondition

  // fundamental data
  vector<Tuple> pattern_;
    
  // computed data
  set<Rule *> rules_; // the rules that have this precondition.
  set<Rule *> feature_rules_; // the feature ones (a subset of the above)
  // The following two indices are for finding rules based on a description:
  // positive rules are indexed by their result
  map<Pattern, set<Rule *> > positive_rule_index_;
  // feature rules are indexed on the target rule. 
  // Given a Precondition of a feature rule, and the rule it features
  // Find all feature rules applying
  // This is a bit odd, as I can't think of a good reason for having multiple
  // feature rules with the same preconditions affecting the same rule.
  // Maybe with tuple encoding...
  map<Rule *, set<Rule *> > feature_rule_index_;
  // The total number of substitutions that satisfy the precondition.
  int num_satisfactions_;
  // maps substitution to satisfaction object
  // some satisfactions are not represented if they are not interesting
  map<Substitution, Satisfaction *> satisfactions_; 
  // Under direct encoding, the encoding cost of the tuples, excluding for
  // universal naming costs accounted for elsewhere.
  double direct_pattern_encoding_ln_likelihood_;

  SearchTree * search_tree_;

  // TUPLE ENCODING STUFF
  // Under direct encoding, the encoding cost of the tuples, excluding
  // universal naming costs accounted for elsewhere.
  //double direct_pattern_encoding_ln_likelihood_;
  // used if the rule is tuple encoded.
  // The rule only comes into the model once the TrueTuples that describe
  // its causes come true.  These are the TrueTuples that describe the
  // rule.
  // TODO, make sure no encoding can be a subset of another encoding.
  //set<TrueTuple *> tuple_causes_;
  // Is this object encoded (paid for) by tuples or directly (using the global
  // naming scheme)
  //bool tuple_encoded_;
};

  
// instance of a precondition being satisfied
class Satisfaction : public Component { 
 public:
  friend class Precondition;
  friend class Rule; 
  friend class RuleSat;
  friend class Firing; 
  friend class TrueTuple;
  friend class Component;
  friend class Model;

  // ----- LAYER 2 FUNCTIONS ----- Satisfaction


  // ----- CONST FUNCTIONS ----- Satisfaction

  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  inline bool NeedsPurpose() const; // yes
  vector<Component *> TemporalDependents() const; // the rule_sats
  vector<Component *> StructuralDependents() const;
  vector<vector<Component *> > TemporalCodependents() const; // Preconditions, tuples
  vector<Component *> Purposes() const; // the rule_sats
  bool HasPurpose() const;
  const set<RuleSat *> & GetRuleSats() const { return rule_sats_;}
  const set<TrueTuple *> GetTrueTuples() const { return true_tuples_;}
  const Substitution & GetSubstitution() const { return substitution_;}

 private:
  // ----- CONSTRUCTOR(S) ----- Satisfaction

  Satisfaction(Precondition * precondition, const Substitution & sub);

  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- Satisfaction

  void L1_EraseSubclass();


  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- Satisfaction

  void A1_AddTrueTuple(TrueTuple *t);
  void A1_RemoveTrueTueple(TrueTuple *t);
  void A1_AddRuleSat(RuleSat *rs);
  void A1_RemoveRuleSat(RuleSat *rs);  
 
  // ----- DATA ----- Satisfaction

  // fundamental data
  Precondition * precondition_;
  Substitution substitution_;

  // computed data
  // The TrueTuples that satisfy the precondition
  set<TrueTuple *> true_tuples_;

  // The associated RuleSat objects for rules which have this precondition.
  // (only the ones that are represented explicitly)
  set<RuleSat *> rule_sats_;
};


class Rule : public Component{
 public:
  friend class Precondition;
  friend class Satisfaction; 
  friend class RuleSat;
  friend class Firing; 
  friend class TrueTuple;
  friend class Component;
  friend class Model;

  // -----LAYER 2 FUNCTIONS ----- Rule
  
  // Adds a firing, possibly also adding a satisfaction, a rulesat, and
  // some truetuples.  If one already exists, just returns it.
  Firing * AddFiring(const Substitution & sub);

  // This is a convenience function, add ALL satisfactions of a rule's
  // preconditions as firings. This must be applied to a simple rule
  void AddAllSatisfactionsAsFirings();

  // Changes the delay on the rule.
  // may leave some times dirty.
  void ChangeDelay(EncodedNumber new_delay);
  // change encoding schemes.

  // TUPLE ENCODING STUFF
  // void SwitchToTupleEncoding();
  // void SwitchToDirectEncoding();


  // ----- CONST FUNCTIONS ----- Rule

  Firing * FindFiring(const Substitution &sub) const;
  // figures out what tuples cause the rule under the tuple encoding.
  // set<Tuple> ComputeTupleCauses() const;
  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  vector<Component *> TemporalDependents() const;
  vector<Component *> StructuralDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Copurposes() const;
  double LnLikelihood() const;
  // Variables in the precondition
  set<int> LeftVariables() const;
  // Variables in the result which are not in the precondition.
  set<int> CreativeVariables() const;
  // Does this rule ever fire?
  bool HasFiring() const;
  // How many times does this rule fire?
  // Does not incude the firings where no decision was necessary
  int NumFirings() const;
  // Number of satisfactions for which this rule fires at least once.
  // Does not incude firings where no decision was necessary.
  int NumFirstFirings() const;
  // Returns all firings for this rule
  vector<Firing *> Firings() const;
  // Displays this rule for the HTML browser
  string ImplicationString() const;
  // Finds a rulesat object if one exists.
  RuleSat * FindRuleSat(Satisfaction * sat) const;
  //  Convenience
  RuleSat * FindRuleSat(const Substitution & sub) const;
  void VerifyLayer2Subclass() const;
  Precondition * GetPrecondition() const { return precondition_;}
  EncodedNumber GetDelay() const { return delay_;}
  const map<Satisfaction *, RuleSat *> & GetRuleSats() const 
    { return rule_sats_;}
  RuleType GetRuleType() const { return type_;}
  const Pattern & GetResult() const { return result_;}
  bool IsUniversalRule() const;
  double GetDirectPatternEncodingLnLikelihood() const {
    return direct_pattern_encoding_ln_likelihood_;
  }
  const set<Rule *> & GetFeatures() const {
    return features_;
  }
  double FirstFiringLikelihoodEstimate(const set<Rule *> & features) const;
  double AdditionalFiringLikelihoodEstimate() const;

  // Computes ln likelihood of the choices of where to have additional firings.
  double AdditionalFiringsLnLikelihood() const;
  


  // TODO: THIS IS A HACK.  SHOULD BE PRIVATE
  map<int, Chooser *> * GetChoosers() { return &choosers_;}

 private:
  // ----- CONSTRUCTOR(S) ----- Rule

  // Create a new rule and add it to the model.
  Rule(Precondition * precondition, EncodedNumber delay,
       RuleType type, Rule * target_rule,
       vector<Tuple> result);
  

  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- Rule

  // gets/adds a RuleSat object for this rule and a particular satisfaction.
  RuleSat * L1_GetAddRuleSat(Satisfaction * sat);
  //  Convenience
  RuleSat * L1_GetAddRuleSat(const Substitution & sub);
  void L1_EraseSubclass();

  // adds to firings_ln_likelihood_ and ln_likelihood_, and model ln likelihood
  void L1_AddToFiringsLnLikelihoodAndVerify(double delta);
  void L1_AddSatisfactionsAndFirstFirings(const set<Rule *> & features,
					  pair<int, int> delta);
  void L1_AddAdditionalFirings(int delta);
  void L1_AddFirstFirings(int delta);

  // computes firings_ln_likelihood_ from scratch.  Doesn't set it. 
  double ComputeFiringsLnLikelihood() const;

  // TUPLE ENCODING STUFF
  //  These functions add and remove the global costs associated with the
  // different encoding schemes.   
  //void L1_MakeDirectlyEncoded();
  //void L1_MakeNotDirectlyEncoded();
  //void L1_MakeTupleEncoded();
  //void L1_MakeNotTupleEncoded();  

  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- Rule

  // Simple L1 Accessors
  void A1_SetDelay(EncodedNumber value);
  void A1_AddRuleSat(Satisfaction * s, RuleSat * rs);
  void A1_RemoveRuleSat(Satisfaction * s);
  void A1_AddFeature(Rule * feature);
  void A1_RemoveFeature(Rule * feature);

  // ----- Data ----- Rule

  // fundamental
  Precondition * precondition_;
  // For positive rules, we need a time delay between the preconditions and
  // the results, so as to make circular causation impossible.  We introduce
  // this delay at the RuleSat (between the satisfaction and the rulesat), 
  // but the "duration" of the delay is associated with the rule, so as to 
  // allow for higher and lower precedence rules. 
  EncodedNumber delay_;
  RuleType type_;
  // postconditions
  vector<Tuple> result_;
  // in the case of a feature rule, the rule that is conditioned
  Rule * target_rule_; 

  // computed
  set<Rule *> features_; // feature rules affecting this one.
  // the RuleSats that are explicitly represented.
  // These are the ones with firings or that have feature rulesats 
  // applying to them, or all RuleSats for a feature rule.
  map<Satisfaction *, RuleSat *> rule_sats_;

  // Maps from set of features affecting a rulesat to number of satisfactions
  // and number of first firings.   
  // These should be uints, but that makes the code too hairy.
  map<set<Rule *>, pair<int, int> > first_firing_counts_;
  // Sum of the number of first firings in the above map.
  uint num_first_firings_;
  // Number of additional firings (after the first ones per rulesat).
  uint num_additional_firings_;
  // The sum of the ln likelihood for all firings (and non-firings) for this 
  // rule.
  double firings_ln_likelihood_;

  // Under direct encoding, the encoding cost of the tuples, excluding
  // universal naming costs accounted for elsewhere.
  double direct_pattern_encoding_ln_likelihood_;
  
  // One chooser for each creative variable.
  map<int, Chooser *> choosers_;
  
  // TUPLE ENCODING STUFF
  // used if the rule is tuple encoded.
  // The rule only comes into the model once the TrueTuples that describe
  // its causes come true.  These are the TrueTuples that describe the
  // rule.
  // TODO, make sure no encoding can be a subset of another encoding.
  //set<TrueTuple *> tuple_causes_;
  // Is this object encoded (paid for) by tuples or directly (using the global
  // naming scheme)
  //bool tuple_encoded_;
};

class RuleSat : public Component{ // an instance of a rule coming true
 public:
  friend class Precondition;
  friend class Rule; 
  friend class Satisfaction; 
  friend class Firing; 
  friend class TrueTuple;
  friend class Component;
  friend class Model;

  // ----- LAYER 2 FUNCTIONS ----- RuleSat
  
  Firing * AddFiring(const Substitution & sub);
  
  // ----- CONST FUNCTIONS ----- RuleSat

  Firing * FindFiring(const Substitution &sub) const;
  int NumFirings() const { return firings_.size(); }
  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  bool NeedsPurpose() const;
  string ImplicationString(const Firing *firing) const;
  vector<Component *> TemporalDependents() const;
  vector<Component *> StructuralDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Purposes() const;
  vector<Component *> Copurposes() const;
  inline bool HasTimeDelay() const { return (rule_->type_!=FEATURE_RULE);}
  inline EncodedNumber GetTimeDelay() const { return rule_->delay_;}
  double LnLikelihood() const;
  bool HasPurpose() const;
  const map<Substitution, Firing *> & GetFirings() const { return firings_;}
  Rule * GetRule() const { return rule_;}
  const Satisfaction * GetSatisfaction() { return satisfaction_;}
  const set<RuleSat *> & GetFeatures() const {
    return features_;
  }
  RuleSat * GetTarget() const { return target_rule_sat_;}
  // Returns the numbers of satisfactions and first firings associated with 
  // this rulesat, for likelihood computation purposes.  The number of 
  // satisfactions is 1, unless it is in the RS_NO_DECISION or the RS_BABY 
  // state.  The number of first firings is 1 if the state is RS_FIRST_FIRING 
  // (at least one firing.
  pair<int, int> SatsAndFirstFirings() const;
  const set<Rule *> & GetTimelyFeatures() const { return timely_features_;}
  const RuleSatState GetState() const { return state_;}
 private:


  // ----- CONSTRUCTOR(S) ----- RuleSat

  RuleSat(Rule * rule, const Substitution & sub);

  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- RuleSat

  void L1_EraseSubclass();
  void F2_AdjustLnLikelihoodForNewTime();
  void F2_ComputeSetTimelyFeatures();
  void F2_ComputeSetState();
  void L1_SetState(RuleSatState new_state);
  void L1_SetTimelyFeatures(const set<Rule *> 
				  & new_timely_features);


  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- RuleSat

  void L1_AddFiring(const Substitution & sub, Firing *f);
  void L1_RemoveFiring(const Substitution & sub);
  void L1_AddFeature(RuleSat *rs);
  void L1_RemoveFeature(RuleSat *rs);

  // ----- DATA ----- RuleSat
  // fundamental

  // The rule that is applied
  Rule * rule_; 

  // the corresponding satisfaction of the preconditions
  Satisfaction * satisfaction_; 

  // computed
  map<Substitution, Firing *> firings_;     // The effects  
  RuleSat * target_rule_sat_; //the conditioned RuleSat if it's a feature rule
  set<RuleSat *> features_; // Feature rule sats inhibiting this one
  set<Rule *> timely_features_; 
  RuleSatState state_;
};

// An instane of a rule firing
class Firing : public Component{
 public:
  friend class Precondition;
  friend class Rule; 
  friend class Satisfaction; 
  friend class RuleSat;
  friend class TrueTuple;
  friend class Component;
  friend class Model;


  // ----- LAYER 2 FUNCTIONS ----- Firing


  // ----- CONST FUNCTIONS ----- Firing

  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  string ImplicationString() const;
  RuleSat * GetRuleSat() const{ return rule_sat_;}
  inline Rule * GetRule() const{ return rule_sat_->rule_; }
  vector<Component *> TemporalDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Copurposes() const;
  Substitution GetFullSubstitution() const;
  const set<TrueTuple *> & GetTrueTuples() const { return true_tuples_;}
  const Substitution & GetRightSubstitution() const 
    { return right_substitution_;}
  
 private:

  // ----- CONSTRUCTOR(S) ----- Firing

  Firing(RuleSat * rule_sat, Substitution right_substitution);
  
  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- Firing

  void L1_EraseSubclass();
  void F2_AdjustLnLikelihoodForNewTime();

  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- Firing

  void A1_AddTrueTuple(TrueTuple *t);
  void A1_RemoveTrueTuple(TrueTuple *t);

  // ----- DATA ----- Firing

  // fundamental
  Substitution right_substitution_; // substitution for new rhs variables
  RuleSat * rule_sat_;  // the corresponding satisfaction of the rule

  // computed
  // vector of true tuples aligned with the result_ of the Rule.
  set<TrueTuple *> true_tuples_;
};

// A tuple which is true in our model
class TrueTuple : public Component{
 public:
  friend class Precondition;
  friend class Rule; 
  friend class Satisfaction; 
  friend class RuleSat;
  friend class Firing; 
  friend class Component;
  friend class Model;
  friend class Prohibition;


  // ----- LAYER 2 FUNCTIONS ----- TrueTuple

  // ----- CONST FUNCTIONS ----- TrueTuple

  ComponentType Type() const;
  Record RecordForDisplaySubclass(bool verbose) const;
  Record RecordForStorageSubclass() const;
  set<Firing *> GetResultFirings() const;
  set<TrueTuple *> GetResultTrueTuples() const;
  set<TrueTuple *> GetCauseTrueTuples() const;
  vector<Component *> TemporalDependents() const;
  vector<Component *> StructuralDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;

  const set<Firing *> & GetCauses() const { return causes_;}
  Firing * GetFirstCause() const;
  int IsRequired() const { return required_count_; }
  const Tuple & GetTuple() const { return tuple_;}
  const set<Satisfaction *> & GetSatisfactions() const { return satisfactions_;}
  
 private:


  // ----- CONSTRUCTOR(S) ----- TrueTuple

  TrueTuple(Model * model, Tuple tuple);


  // ----- COMPLICATED LAYER 1 FUNCTIONS ----- TrueTuple

  void L1_EraseSubclass();
  void F2_AdjustLnLikelihoodForNewTime();

  // ----- LAYER 1 ACCESSOR FUNCTIONS ----- TrueTuple

  void A1_AddViolatedProhibition(Prohibition *p);
  void A1_RemoveViolatedProhibition(Prohibition *p);
  void A1_MakeRequired(); // increases the required_ count by 1.
  void A1_MakeNotRequired(); // decreases the required_ count by 1.
  void A1_AddCause(Firing *cause);
  void A1_RemoveCause(Firing *cause);
  void A1_AddSatisfaction(Satisfaction *sat);
  void A1_RemoveSatisfaction(Satisfaction *sat);
  
  // ----- DATA ----- TrueTuple
  // fundamental
  Tuple tuple_;  

  // computed
  // the firings that would make this tuple true 
  // (though only the temporally first really does).
  set<Firing *> causes_;

  // Satisfactions in which this proposition takes part.
  set<Satisfaction *> satisfactions_;

  // What rules does this Tuple help encode?
  //set<Rule *> rules_caused_;
  
  // How many things require this tuple to be true.
  int required_count_;

  // prohibitions we are violating (from the problem spec, or from the future 
  // tuple encoding stuff).
  set<Prohibition *> violated_prohibitions_;

};

// TUPLE ENCODING STUFF
// Returns the representation of a tuple (a clause in a rule).
// Used in determining the encoding of rules.
// vector<Tuple> ComputeTupleEncoding(const Tuple &s, int name);


#endif
