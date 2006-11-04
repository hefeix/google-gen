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
//           f. All satisfactions of negative rules have their rulesats
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

class Model;
class Component; //  a basic part of the model.  There are several subtypes.

// These are the types of components
class Precondition;  // The preconditions of a rule
class Rule;
class Satisfaction; // An instance of a Precondition being satisfied.
class RuleSat;      // An instance of a rule being satisfied.
class Firing;       // An instance of a rule firing.
class TrueTuple;

// Something else entirely
class Prohibition;

#define ADD_FRIEND_COMPONENT_CLASSES friend class Precondition; \
  friend class Rule; friend class Satisfaction; friend class RuleSat;	\
  friend class Firing; friend class TrueTuple;

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
static char * ComponentTypeName []; // indexed by the enum above. 
static ComponentType StringToComponentType(const string & s); 
static string ComponentTypeToString(ComponentType t);

// These are the types of rules.
enum RuleType {
  INVALID_RULE, // not used
  SIMPLE_RULE,  // no new variables on RHS.  Can fire only once per rule_sat
  NEGATIVE_RULE, // Inhibits another rule. 
  CREATIVE_RULE, // New variables on RHS.  Can have multiple firings.
  NUM_RULE_TYPES,
};
static char * RuleTypeName [];
static RuleType StringToRuleType(const string & s);
static string RuleTypeToString(RuleType t);

class Component{
 public:
  ADD_FRIEND_COMPONENT_CLASSES;


  // ----- LAYER 2 FUNCTIONS -----

  // This function makes the component not exist.  It also first erases its
  // StructuralDependents, and its Copurposes that no loner have a purpose.  
  // You can call Erase() on anything but Satisfaction and RuleSat objects.
  // Those can't be removed in a Layer 2 manner (since all rule_sats of 
  // a negative rule must be represented), so call L1_Erase.
  void Erase(); // but don't delete.
  // The time_ field is set lazily to save time.  Sometimes it is locally 
  // correct (clean) and sometimes it might be localy incorrect (dirty).  
  // Clean does not mean globally correct, and calling ComputeSetTime()
  // on every component does not make all the times clean or correct.
  // Call FixTimes() on the model to make all of the times clean and correct.
  // This function computes and sets the time for this component, and makes
  // the dirty bit for this component false.  It may make the dirty bits 
  // for other components true.
  void ComputeSetTime(); 
  // Computes and sets the ln_likelihood_ for this component, and adjusts
  // the total ln likelihood of the model. 
  void ComputeSetLnLikelihood();
  // If the time of a component changes, we call this function, which 
  // adjusts the ln_likelihood_ of this and other components if necessary.
  virtual void AdjustLnLikelihoodForNewTime();


  // ----- CONST FUNCTIONS -----

  Model * GetModel() const { return model_;}

  virtual ComponentType Type() const = 0;
  string TypeName() const;

  void VerifyLayer2() const;
  virtual void VerifyLayer2Subclass() const;
  
  // A link to this component in the HTML viewer.
  string HTMLLink(string text) const;
  
  // Key-Value pairs to be displayed in the HTML viewer. 
  Record RecordForDisplay() const;
  
  // Type-specific key-value pairs.  Called by RecordForDisplay()
  virtual Record RecordForDisplayInternal() const = 0;
  
  // Returns pointers to the components that depend in some way on this 
  // component.  They may have other options for existing, so they don't 
  // necessarily disappear when this component does.
  virtual vector<Component *> TemporalDependents() const;
  
  // TODO: maybe get rid of this
  // Components that absolutely need to disappear before this one does.
  // The code breaks if you remove this component while some of these exist.
  virtual vector<Component *> HardTemporalDependents() const;

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

 private:


  // ----- CONSTRUCTOR(S) -----

  // The constructor should leave the exists_ bit false, and the destructor
  // should CHECK that it is false.  The constructor and destructor should 
  // not touch the rest of the model.
  Component(Model * model);
  virtual ~Component();


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
  // This function is layer 2, except on Satisfaction and negative RuleSat 
  // objects.
  void L1_Erase();
  // the subclass-specific parts of the erase function.
  virtual void L1_EraseSubclass() = 0;

  // flips the dirty bit on, and inserts into global set of dirty components.
  void L1_MakeTimeDirty();
  // flips the dirty bit off, and removes from global set of dirty components.
  void L1_MakeTimeClean();

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  // These things just modify the actual values.
  void A1_SetExists(bool val);
  void A1_SetTime(const Time & new_time);
  void A1_SetTimeDirty(bool val); // Use the L1 functions above instead!
  void A1_SetLnLikelihood(double new_ln_likelihood); 


  // ----- DATA -----

  // All data should only be touched by reversible functions.
  
  // If we delete a component and want to restore it on rollback, we want to
  // avoid invalidating pointers and storing the data elswhere.  Thus we just 
  // set the exists_ variable to false and pretend that it doesn't exist.
  bool exists_;
  int id_;
  Model * model_;
  // contribution of this component to the ln_likelihood_ of the model
  double ln_likelihood_;
  Time time_;
  // time_dirty_ is set to true unless the time is set correctly based on the 
  // codependents of this component (though not necessarily globally).
  bool time_dirty_;
};

// Here we have the six types of components.

// A Precondition represents the preconditions of a rule, which are a vector
// of tuples containing variables.
// The ln_likelihood_ aggregates the likelihood of all of its rules never
// firing.
class Precondition : public Component {
 public:
  ADD_FRIEND_COMPONENT_CLASSES;

  // ----- LAYER 2 FUNCTIONS -----
  
  // TUPLE ENCODING STUFF
  // change encoding schemes.
  //void SwitchToTupleEncoding();
  //void SwitchToDirectEncoding();
  
  // ----- CONST FUNCTIONS -----

  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  bool HasPurpose() const;
  vector<Component *> Purposes() const; // Rules
  vector<Component *> TemporalDependents() const; // Rules, Satisfactions
  bool NeedsPurpose() const; // yes
  double LnLikelihood() const;
  // returns pointer to a satisfaction object if one exists.
  Satisfaction * FindSatisfaction(const Substitution &sub) const;
  virtual void VerifyLayer2Subclass() const;
  Rule * FindPositiveRule(const vector<tuple> & result) const;
  Rule * FindNegativeRule(Rule * target_rule) const;

  // TUPLE ENCODING STUFF
  // figures out what tuples cause the precondition under the tuple encoding.
  // set<Tuple> ComputeTupleCauses() const;


 private:
  // ----- CONSTRUCTOR(S) -----

  Precondition(Model * model, const vector<Tuple> & tuples, int id);


  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

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

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_AddRule(Rule *r);
  void A1_RemoveRule(Rule *r);
  void A1_AddNegativeRule(Rule *r);
  void A1_RemoveNegatieveRule(Rule *r);
  void A1_AddSatisfaction(Satisfaction * sat);
  void A1_RemoveSatisfaction(Satisfaction *sat);
  void A1_SetPreconditionLnLikelihood(double val);
  void A1_SetLnLikelihoodPerSat(double val);
  // Changes the total number of satisfactions (including ones not represented)
  void A1_AddToNumSatisfactions(int delta);


  // ----- DATA -----

  // fundamental data
  vector<Tuple> pattern_;
    
  // computed data
  set<Rule *> rules_; // the rules that have this precondition.
  set<Rule *> negative_rules_; // the negative ones (a subset of the above)
  // The following two indices are for finding rules based on a description:
  // positive rules are indexed by their result
  map<Pattern, set<Rule *> > positive_rule_index_;
  // negative rules are indexed on the inhibited rule. 
  map<Rule *, set<Rule *> > negative_rule_index_;
  // The total number of substitutions that satisfy the precondition.
  int num_satisfactions_;
  // maps substitution to satisfaction object
  // some satisfactions are not represented if they are not interesting
  map<Substitution, Satisfaction *> satisfactions_; 
  // Under direct encoding, the encoding cost of the tuples, excluding for
  // universal naming costs accounted for elsewhere.
  double direct_pattern_encoding_ln_likelihood_;
  // The additional ln likelihood added to the model for each satisfaction
  // of the precondition for which none of the associated rules are satisfied.
  double ln_likelihood_per_sat_;

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
  ADD_FRIEND_COMPONENT_CLASSES;

  // ----- LAYER 2 FUNCTIONS -----


  // ----- CONST FUNCTIONS -----

  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  inline bool NeedsPurpose() const; // yes
  vector<Component *> TemporalDependents() const; // the rule_sats
  vector<vector<Component *> > TemporalCodependents() const; // Preconditions, tuples
  vector<Component *> Purposes() const; // the rule_sats
  bool HasPurpose() const;


 private:
  // ----- CONSTRUCTOR(S) -----

  Satisfaction(Precondition * precondition, const Substitution & sub,
	       int id);


  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  void L1_EraseSubclass();


  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_AddTrueTuple(TrueTuple *t);
  void A1_RemoveTrueTueple(TrueTuple *t);
  void A1_AddRuleSat(RuleSat *rs);
  void A1_RemoveRuleSat(RuleSat *rs);  
 

  // ----- DATA -----

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
  ADD_FRIEND_COMPONENT_CLASSES;

  // -----LAYER 2 FUNCTIONS -----
  
  // Adds a firing, possibly also adding a satisfaction, a rulesat, and
  // some truetuples.  If one already exists, just returns it.
  Firing * AddFiring(const Substitution & sub);
  // Changes the strength of the rule, keeping the model likelihood updated
  void ChangeStrength(EncodedNumber new_strength, 
		      EncodedNumber new_strength2);
  // Changes the delay on the rule.
  // may leave some times dirty.
  void ChangeDelay(EncodedNumber new_delay);
  // change encoding schemes.

  // TUPLE ENCODING STUFF
  // void SwitchToTupleEncoding();
  // void SwitchToDirectEncoding();


  // ----- CONST FUNCTIONS -----

  Firing * GetFiring(const Substitution &sub) const;
  // figures out what tuples cause the rule under the tuple encoding.
  // set<Tuple> ComputeTupleCauses() const;
  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  vector<Component *> TemporalDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Copurposes() const;
  double LnLikelihood() const;
  // Variables in the precondition
  set<int> LeftVariables() const;
  // Variables in the result which are not in the precondition.
  set<int> RightVariables() const;
  // Does this rule ever fire?
  bool HasFiring() const;
  // How many times does this rule fire?
  int NumFirings() const;
  // Number of satisfactions for which this rule fires at least once.
  int NumFirstFirings() const;
  // Returns all firings for this rule
  vector<Firing *> Firings() const;
  // Displays this rule for the HTML browser
  string ImplicationString() const;
  // Finds a rulesat object if one exists.
  RuleSat * FindRuleSat(Satisfaction * sat) const;
  //  Convenience
  RuleSat * FindRuleSat(const Substitution & sub) const;
  virtual void VerifyLayer2Subclass() const;

 private:
  // ----- CONSTRUCTOR(S) -----

  // Create a new rule and add it to the model.
  Rule(Precondition * precondition, EncodedNumber delay,
       RuleType type, Rule * target_rule,
       vector<Tuple> result, 
       EncodedNumber strength, EncodedNumber strength2);
  

  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  // gets/adds a RuleSat object for this rule and a particular satisfaction.
  RuleSat * L1_GetAddRuleSat(Satisfaction * sat);
  //  Convenience
  RuleSat * L1_GetAddRuleSat(const Substitution & sub);
  void L1_EraseSubclass();

  // TUPLE ENCODING STUFF
  //  These functions add and remove the global costs associated with the
  // different encoding schemes.   
  //void L1_MakeDirectlyEncoded();
  //void L1_MakeNotDirectlyEncoded();
  //void L1_MakeTupleEncoded();
  //void L1_MakeNotTupleEncoded();  

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  // Simple L1 Accessors
  void A1_SetDelay(EncodedNumber value);
  void A1_SetStrength(EncodedNumber value);
  void A1_SetStrength2(EncodedNumber value);
  void A1_SetStrengthD(double value);
  void A1_SetStrength2D(double value);
  void A1_AddRuleSat(Satisfaction * s, RuleSat * rs);
  void A1_RemoveRuleSat(Satisfaction * s, RuleSat * rs);
  void A1_SetRuleLnLikelihood(double val);
  void A1_AddInhibitor(Rule * inhibitor);
  void A1_RemoveInhibitor(Rule * inhibitor);

  // ----- DATA -----

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
  // If the rule is simple or creative, strength_ is the probability for each
  // satisfaction that the rule fires at least once.
  // If the rule is negative, 1-strength_ is a multiplier on the prior of the
  // target rule_sat having at least one firing. 
  EncodedNumber strength_;
  // If the rule is creative, then it fires a number of times given by 
  // a geometric distribution with parameter (1-strength2_), i.e. if it has 
  // fired at least once, it has a probability of strength2_ of firing again.
  // Otherwise, this value is not used.
  EncodedNumber strength2_;
  // in the case of a negative rule, the rule that is inhibited
  Rule * target_rule_; 

  // computed
  set<Rule *> inhibitors_; // negative rules affecting this one.
  // floating point representation of the rule strength
  double strength_d_; 
  double strength2_d_;
  // the RuleSats that are explicitly represented.
  // These are the ones with firings or that are inhibited, or all RuleSats
  // for a negative rule.
  map<Satisfaction *, RuleSat *> rule_sats_;

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

class RuleSat : public Component{ // an instance of a rule coming true
 public:
  ADD_FRIEND_COMPONENT_CLASSES;


  // ----- LAYER 2 FUNCTIONS -----
  
  Firing * AddFiring(const Substitution & sub);
  
  // ----- CONST FUNCTIONS -----

  Firing * GetFiring(const Substitution &sub) const;
  int NumFirings() const { return firings_.size(); }
  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  bool NeedsPurpose() const;
  string ImplicationString(const Firing *firing) const;
  vector<Component *> TemporalDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Purposes() const;
  vector<Component *> Copurposes() const;
  inline bool HasTimeDelay() const { return (rule_->type_!=NEGATIVE_RULE);}
  inline EncodedNumber GetTimeDelay() const { return rule_->delay_;}
  double LnLikelihood() const;
  bool HasPurpose() const;

 private:


  // ----- CONSTRUCTOR(S) -----

  RuleSat(Rule * rule, const Substitution & sub);

  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  void L1_EraseSubclass();
  void F2_AdjustLnLikelihoodForNewTime();

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_AddFiring(Substitution sub, Firing *f);
  void A1_RemoveFiring(Substitution sub);
  void A1_AddInhibitor(RuleSat *rs);
  void A1_RemoveInhibitor(Rulesat *rs);

  // ----- DATA -----
  // fundamental

  // The rule that is applied
  Rule * rule_; 

  // the corresponding satisfaction of the preconditions
  Satisfaction * satisfaction_; 

  // computed
  map<Substitution, Firing *> firings_;     // The effects  
  RuleSat * target_rule_sat_; // the inhibited RuleSat if it's a negative rule
  set<RuleSat *> inhibitors_; // Negative rule sats inhibiting this one
};

// An instane of a rule firing
class Firing : public Component{
 public:
  ADD_FRIEND_COMPONENT_CLASSES;


  // ----- LAYER 2 FUNCTIONS -----


  // ----- CONST FUNCTIONS -----

  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  string ImplicationString() const;
  RuleSat * GetRuleSat() const{ return rule_sat_;}
  inline Rule * GetRule() const{ return rule_sat_->rule_; }
  vector<Component *> TemporalDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;
  vector<Component *> Copurposes() const;
  Substitution GetFullSubstitution() const;

 private:


  // ----- CONSTRUCTOR(S) -----

  Firing(RuleSat * rule_sat, Substitution right_substitution);
  
  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  void L1_EraseSubclass();

  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_AddTrueTuple(TrueTuple *t);
  void A1_RemoveTrueTuple(TrueTuple *t);

  // ----- DATA -----

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
  ADD_FRIEND_COMPONENT_CLASSES;


  // ----- LAYER 2 FUNCTIONS -----

  void AddCause(Firing * cause);
  void RemoveCause(Firing * cause);

  // ----- CONST FUNCTIONS -----

  ComponentType Type() const;
  Record RecordForDisplayInternal() const;
  set<Firing *> GetResultFirings() const;
  set<TrueTuple *> GetResultTrueTuples() const;
  set<TrueTuple *> GetCauseTrueTuples() const;
  vector<Component *> TemporalDependents() const;
  vector<vector<Component *> > TemporalCodependents() const;

 private:


  // ----- CONSTRUCTOR(S) -----

  TrueTuple(Model * model, Tuple tuple);


  // ----- COMPLICATED LAYER 1 FUNCTIONS -----

  void L1_EraseSubclass();


  // ----- LAYER 1 ACCESSOR FUNCTIONS -----

  void A1_AddViolatedProhibition(Prohibition *p);
  void A1_RemoveViolatedProhibition(Prohibition *p);
  void A1_MakeRequired(); // increases the required_ count by 1.
  void A1_MakeNotRequired(); // decreases the required_ count by 1.
  void A1_AddCause(Firing *cause);
  void A1_RemoveCause(Firing *cause);
  void A1_AddSatisfaction(Satisfaction *sat);
  void A1_RemoveSatisfaction(Satisfaction *sat);
  
  // ----- DATA -----
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
