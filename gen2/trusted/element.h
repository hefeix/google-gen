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

#ifndef _ELEMENT_H_
#define _ELEMENT_H_

#include "named.h"
#include "link.h"
#include "extensions.h"


struct Element : public Named {  
  public:

  #define ElementFunctionList {			\
      ITEM(PASS),					\
	ITEM(ON),					\
	ITEM(REPEAT),					\
	ITEM(DELAY),					\
	ITEM(LET),					\
	ITEM(OUTPUT),					\
	ITEM(IF),					\
	ITEM(PARALLEL),					\
	ITEM(SUBSTITUTE),				\
	ITEM(FLAKE_CHOICE),				\
	ITEM(CONSTANT),					\
	};
  CLASS_ENUM_DECLARE(Element, Function);

  // ---------- L2 functions ----------  

  // ---------- const functions ----------  

  virtual bool IsDynamic() const = 0;
  virtual OMap GetBinding() const { CHECK(false); return OMap();}
  // computes the proper time of this element. 
  virtual OTime ComputeTime() const = 0;
  // gets the current time of an element.
  OTime GetTime() const { return time_;}
  // computes the proper time of a child element. 
  virtual OTime ComputeChildTime(const Link * link, const Element *child) const{
    return time_;    
  }
  Record GetRecordForDisplay() const;
  virtual Element * GetParent() const = 0;
  // a human and machine readable version of the static subtree
  virtual string ToString(bool html) const = 0;  
  virtual Function GetFunction() const = 0;
  virtual Keyword FunctionKeyword() const { 
    return Keyword::Make(Downcase(FunctionToString(GetFunction())));
  }
  int WhichChild() { if (parent_) return parent_->WhichChild(); return -1; }
  
  // ---------- L1 functions ----------  
  Element() :parent_(NULL){};
  virtual void L1_Init() { Named::L1_Init(); }
  void L1_Erase();

  // these functions are only called by the link. 
  // Outsiders should call Link::L1_AddChild() or Link::L1_RemoveChild().
  virtual void L1_ConnectToParentLink (Link * link) = 0;
  virtual void L1_DisconnectFromParentLink(Link * link) = 0;

  // call this if the computed time may have changed. 
  // creates a time violation (or destroys one) as necessary. 
  void N1_TimeMayHaveChanged(); 

  // ---------- data ----------  

  Link * parent_;
  OTime time_;
};

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  // ---------- data ----------  


struct Statement;
struct Expression;
struct DynamicStatement;
struct DynamicExpression;

struct StaticElement : public Element {
  // ---------- L2 functions ----------

  // TODO: implement these functions
  void UnlinkFromParent();
  void LinkToParent(StaticElement *parent, int which_child);
  void EraseTree();  
  void SetObject(int which, Object new_value);

  // ---------- const functions ----------  
  bool IsDynamic() const { return false; }
  OTime ComputeTime() const { return CREATION;}
  StaticElement * GetParent() const;
  StaticElement * GetChild(int which) const;
  DynamicElement * GetDynamic(OMap binding) const;
  Statement * GetStatementChild(int which) const;// which < NumStatementChildren
  Expression * GetExpressionChild(int which) const;
  vector<Statement *> GetStatementChildren() const;
  Object GetObject(int which) const;
  virtual int NumExpressionChildren() const = 0;
  virtual int NumChildren() const = 0;
  int NumStatementChildren()  const{ 
    return NumChildren() - NumExpressionChildren(); }
  Named::Type ChildType(int which) {
    CHECK(which>=0 && which < NumChildren());
    return (which < NumExpressionChildren())
      ?Named::EXPRESSION:Named::STATEMENT;
  }
  int NumDynamicChildren() const { return dynamic_children_->children_.size();}
  virtual int NumObjects() const { return 0;}
  // the objects and expression children
  string ParameterListToString(bool html) const; 
  // What new variables are introduced by this node
  virtual set<Variable> GetIntroducedVariables() const;
  // What variables exist at this node. 
  // This is the union of GetIntroducedVariables() up the static tree, 
  // not including this node. 
  virtual set<Variable> GetVariables() const;
  Record GetRecordForDisplay() const;

  // ---------- L1 functions ----------  
  void L1_Init();
  void L1_Erase();

  void L1_LinkChild(int where, StaticElement *child);
  void L1_UnlinkChild(int where);
  void L1_SetObject(int which, Object new_value);

  //This should only be called from Link::L1_AddChild()
  void L1_ConnectToParentLink(Link *link) { CL.ChangeValue(&parent_, link);}
  //This should only be called from Link::L1_RemoveChild()
  void L1_DisconnectFromParentLink(Link *link) { 
    CHECK(parent_ == link);
    CL.ChangeValue(&parent_, (Link *) NULL);
  }
  // ---------- N1 notifiers ----------  
  virtual void N1_ObjectChanged(int which) {}

  // ---------- data ----------  

  // these links (like all links) are owned by the parent
  MultiLink * dynamic_children_;
  vector<SingleLink *> static_children_; // statements and expressions
  vector<Object> objects_;


};

struct DynamicElement : public Element{

  // ---------- L2 functions ----------  
  bool SetBinding(OMap new_binding); // returns false on failure
  // it can figure out the parent from the static node and the bindings.
  // returns false on failure.
  bool LinkToParent(); 
  void UnlinkFromParent();


  // ---------- const functions ----------  
  bool IsDynamic() const { return true; }
  DynamicElement * GetParent() const { 
    if (!parent_) return NULL;
    return dynamic_cast<DynamicElement *>(parent_->GetParent());
  }
  StaticElement * GetStatic() const {
    return dynamic_cast<StaticElement *>(static_parent_->parent_); 
  }
  int NumExpressionChildren() const { 
    return GetStatic()->NumExpressionChildren();}
  int NumChildren() const { return GetStatic()->NumChildren();}
  int NumStatementChildren() const { 
    return GetStatic()->NumStatementChildren();}
  int NumObjects() const { return GetStatic()->NumObjects();}  
  Object GetObject(int which) const { return GetStatic()->GetObject(which);}
  DynamicElement * GetSingleChild(int which) const;
  DynamicExpression * GetSingleExpressionChild(int which) const;
  DynamicStatement * GetSingleStatementChild(int which) const;
  OMap GetBinding() const { return binding_;}
  DynamicElement * FindParent() const; // finds parent based on bindings.
  virtual Link::Type LinkType(int which_child) const { return Link::SINGLE;}
  OTime ComputeTime() const;
  string ToString(bool html) const;
  virtual Function GetFunction() const { return GetStatic()->GetFunction();}
  virtual OMap ComputeNewBinding() { return OMap::Default(); }
  // static parent and binding need to exist. binding needs to be correct
  // when restricted to the variables of the parent of the static parent. 
  Link * FindDynamicParentLink();

  // ---------- L1 functions ----------  
  // we probably only need two of these parameters, since we can figure out 
  // the third, but we'd rather pass all three here than worry about the corner
  // cases at this point. 
  void L1_Init(StaticElement * static_parent, OMap binding);
  void L1_Erase() { Element::L1_Erase();}
  // should only be called by Link::L1_AddChild();
  void L1_ConnectToParentLink(Link *link) {
    if (link->parent_->IsDynamic())
      parent_ = link;
    else static_parent_ = dynamic_cast<MultiLink*>(link);
  }
  // should only be caled by Link::L1_RemoveChild()
  void L1_DisconnectFromParentLink(Link *link) { 
    if (link->parent_->IsDynamic())
      CL.ChangeValue(&parent_, (Link *)NULL);
    else 
      CL.ChangeValue(&static_parent_, (MultiLink *)NULL);
  }


  // checks for violations
  void L1_CheckSetParentAndBindingViolations();
  
  // ---------- N1 notifiers ----------  

  // this gets called when the value of a child expression changes, 
  // or when a child expresison is created(Init) or destroyed(L1_Erase).   
  virtual void N1_ChildExpressionChanged(int which_child) = 0;

  // The following are called externally when a child is connected to
  // or disconnected from this parent node. 
  virtual void N1_ChildConnected(Link *child_link, DynamicElement *child);
  virtual void N1_ChildDisconnected(Link *child_link, DynamicElement *child);


  // ---------- data ----------  
  MultiLink * static_parent_;
  vector<Link *> children_;
  OMap binding_; // if parent_ is a multilink, always matches it. 
  
};

struct Statement : public StaticElement{
  // ---------- L2 functions ----------  
  // TODO: actually make this thing L2
  static Statement * MakeStatement(Keyword type);
  
  // ---------- const functions ----------  
  Named::Type GetNamedType() const { return Named::STATEMENT; }
  string ToString(bool html) const { return ToString(0, html);}
  string ToString(int indent, bool html) const; // includes the subtree
  string ToStringSingle(bool html) const;

  // ---------- L1 functions ----------  
  virtual ~Statement(){}
  void L1_Init(); // shadow constructor
  // Erasing. Can only erase statements with no children.
  virtual void L1_Erase();

  // ---------- data ----------  
};

struct Expression : public StaticElement {  
  // ---------- L2 functions ----------  
  // TODO: actually make this thing L2
  static Expression * MakeExpression(Keyword type);
  // ---------- const functions ----------  
  string ToString(bool html) const;
  Named::Type GetNamedType() const { return Named::EXPRESSION;}
  virtual int NumExpressionChildren() const { return NumChildren();}
  virtual int NumChildren() const = 0;
  virtual int NumObjects() const { return 0;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};




struct DynamicStatement : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Named::Type GetNamedType() const { return Named::DYNAMIC_STATEMENT;}
  // ---------- L1 functions ----------  
  void L1_Erase() { DynamicElement::L1_Erase();}
  void L1_Init(StaticElement * static_parent, OMap binding){
    DynamicElement::L1_Init(static_parent, binding);
  }
  // ---------- data ----------  
};

struct DynamicExpression : public DynamicElement {
  // ---------- L2 functions ----------  
  void SetValue(Object new_value);
  // ---------- const functions ----------  
  virtual Object ComputeValue() const = 0;
  Object GetValue() const { return value_;}
  Named::Type GetNamedType() const { return Named::DYNAMIC_EXPRESSION;}
  // ---------- L1 functions ----------  
  void L1_Init(Expression * static_parent, OMap binding);
  void L1_Erase();
  void N1_ChildExpressionChanged(int which_child) { 
    L1_CheckSetValueViolation();}
  // checks whether the value is correct, and creates/removes a violation.
  void L1_CheckSetValueViolation();
  // ---------- data ----------  
  Object value_;
};


struct StaticPass : public Statement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return 0;}
  int NumChildren() const { return 0;}
  int NumObjects() const { return 0;}
  virtual Function GetFunction() const { return PASS;}
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};
struct DynamicPass : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  void N1_ChildExpressionChanged(int which_child) { CHECK(false);}
  // ---------- data ----------  
};

struct StaticOn : public Statement {
  enum {
    // expressions
    // statements
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    PATTERN, // OPattern
    NUM_OBJECTS,
  };

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  OPattern GetPattern() const { return GetObject(PATTERN);}
  virtual Function GetFunction() const { return ON;}
  set<Variable> GetIntroducedVariables() const;
  // ---------- L1 functions ----------  
  void L1_Init();
  void L1_Erase();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){CHECK(NumDynamicChildren() == 0);}

  // ---------- data ----------  
  
};

struct DynamicOn : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Link::Type LinkType(int which_child) const{ return Link::ON;}
  StaticOn *GetStaticOn() const { 
    return dynamic_cast<StaticOn*>(GetStatic());
  }
  OPattern GetPattern() const { return GetStaticOn()->GetPattern();}
  OTime ComputeChildTime(const Link * link, const Element *child) const;
  // ---------- L1 functions ----------  
  void L1_Init(StaticOn* parent);
  void L1_Erase();
  // --------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { CHECK(false); }
  // ---------- data ----------  
};


struct StaticRepeat : public Statement {
  enum {
    // expressions
    REPETITIONS,
    // statements
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    REPETITION_VARIABLE,
    NUM_OBJECTS,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  Variable GetRepetitionVariable() const { 
    return GetObject(REPETITION_VARIABLE);}
  virtual Function GetFunction() const { return REPEAT;}
  set<Variable> GetIntroducedVariables() const {
    return SingletonSet(GetRepetitionVariable()); }
  // ---------- L1 functions ----------  
  void L1_Init();

  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){CHECK(NumDynamicChildren() == 0);}
};

struct DynamicRepeat : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  virtual Link::Type LinkType(int which_child) const { 
    return (which_child==StaticRepeat::CHILD)?Link::MULTI:Link::SINGLE;
  }
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { 
    // todo: check whether the number of repetitions is correct
    CHECK(false);
  }
  // ---------- data ----------  
};


struct StaticDelay : public Statement { 
  enum {
    // expressions
    DIMENSION,
    // statements
    CHILD,
    NUM_CHILDREN,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  Function GetFunction() const { return DELAY;}

  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------    
};
struct DynamicDelay : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  OTime ComputeChildTime(const Link * link, const Element *child) const {
    if (link == children_[StaticDelay::CHILD]) {
      return OTime::Make
	(time_.Data() 
	 + OBitSeq
	 (GetSingleExpressionChild(StaticDelay::DIMENSION)->value_).Data());
      }
    return time_;
  }
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { 
    // todo: check whether the times are right on the children
    CHECK(false);
  }
  // ---------- data ----------  
};
  
struct StaticLet : public Statement {
  enum {
    // expressions
    VALUE,
    // statements
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    VARIABLE,
    NUM_OBJECTS,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  Variable GetVariable() const { return GetObject(VARIABLE);}
  virtual Function GetFunction() const { return LET;}

  set<Variable> GetIntroducedVariables() const {
    return SingletonSet(GetVariable());}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){CHECK(NumDynamicChildren() == 0);}
  // ---------- data ----------  
};
struct DynamicLet : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  bool HasLetViolation() const; // a let violation should exist.
  // ---------- L1 functions ----------  
  void L1_CheckSetLetViolation(); // (programming with Dr. Seuss)
  void L1_Init(StaticLet * static_parent, OMap binding) {
    DynamicStatement::L1_Init(static_parent, binding);
    L1_CheckSetLetViolation();
  }
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { 
    L1_CheckSetLetViolation();
  }
  // ---------- data ----------  
};

struct StaticOutput : public Statement {
  enum {
    // expressions
    TUPLE,
    // statements
    NUM_CHILDREN,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return NUM_CHILDREN;}
  int NumChildren() const { return NUM_CHILDREN;}
  virtual Function GetFunction() const { return OUTPUT;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};
struct DynamicOutput : public DynamicStatement {
  // ---------- L2 functions ----------  
  void AddCorrectPosting();
  void AddPosting(OTuple t);
  void RemovePosting();
  // ---------- const functions ----------  
  bool IsPerfect() const;
  DynamicExpression * GetTupleExpression() const {
    return dynamic_cast<DynamicExpression *>
      (GetSingleChild(StaticOutput::TUPLE));
  }  
  // ---------- L1 functions ----------  
  void L1_Init(StaticLet * static_parent, OMap binding) {
    DynamicStatement::L1_Init(static_parent, binding);
    L1_CheckSetPostingViolation();
  }
  // see if it's perfect, then create or remove violation if necessary.
  void L1_CheckSetPostingViolation();
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { 
    L1_CheckSetPostingViolation();
  }  
  // ---------- data ----------  
  OwnedPosting * posting_;
};

struct StaticIf : public Statement {
  enum {
    //expressions
    CONDITION,
    // statements
    ON_TRUE,
    ON_FALSE,
    NUM_CHILDREN,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return ON_TRUE;}
  int NumChildren() const { return NUM_CHILDREN;}
  Function GetFunction() const { return IF;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};
struct DynamicIf : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  DynamicExpression * GetConditionExpression() const {
    return dynamic_cast<DynamicExpression *>
      (GetSingleChild(StaticIf::CONDITION));
  }  
  bool IsPerfect() const;
  // ---------- L1 functions ----------  
  void L1_CheckSetIfViolation();
  void L1_Init(StaticIf * static_parent, OMap binding) {
    DynamicStatement::L1_Init(static_parent, binding);
    dynamic_cast<SingleLink *>(children_[StaticIf::ON_TRUE])
      ->L1_SetOptional(true);
    dynamic_cast<SingleLink *>(children_[StaticIf::ON_FALSE])
      ->L1_SetOptional(true);
    L1_CheckSetIfViolation();
  }
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) {
    // todo: check that the correct child is instantiated
    // todo: what state of the world means that a child is not instantiated?
    //       does the link just point to null. If so, then MISSING_LINK
    //       violations are not violations
    CHECK(false);
  }
  // ---------- data ----------  
};

struct StaticParallel : public Statement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return 0;}
  int NumChildren() const { return static_children_.size(); }
  virtual Function GetFunction() const { return PARALLEL;}

  // ---------- L1 functions ----------  
  void L1_Init() { Statement::L1_Init();}
  // ---------- data ----------  
};

struct DynamicParallel : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionChanged(int which_child) { CHECK(false); }
  // ---------- data ----------  
};


struct StaticSubstitute : public Expression {
  enum {
    CHILD,
    NUM_CHILDREN,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumChildren() const { return NUM_CHILDREN;}
  virtual Function GetFunction() const { return SUBSTITUTE;}

  // ---------- L1 functions ----------  
  void L1_Init() {Expression::L1_Init();}
  // ---------- data ----------  

};
struct DynamicSubstitute : public DynamicExpression {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Object ComputeValue() const;
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};

struct StaticFlakeChoice : public Expression { 
  enum {
    CHOOSER, 
    NUM_CHILDREN,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumChildren() const { return NUM_CHILDREN;}
  virtual Function GetFunction() const { return FLAKE_CHOICE;}

  // ---------- L1 functions ----------  
  void L1_Init() {Expression::L1_Init();}
  // ---------- data ----------  
};
struct DynamicFlakeChoice : public DynamicExpression {
  // ---------- L2 functions ----------  

  // ---------- const functions ----------  
  Object ComputeValue() const;

  // ---------- L1 functions ----------  
  void L1_ChangeChooser(Object new_chooser_name);
  void L1_ChangeChoice(Flake new_choice);
  private:
  // this function is called only from L1_ChangeChooser and L1_ChangeChoice
  // and add/remove the current choice to the current chooser
  void L1_AddToChooser(int count_delta);
  public:

  // ---------- data ----------  
  // Both of the following can be set to NULL, which is a violation,
  //  but can be used to avoid creating unnecessary transient c_objects. 
  Object chooser_name_; // which of the model's flake_choosers_ do we use. 
  Flake choice_; // The current choice.    
};

struct StaticConstant : public Expression {
  enum {
    NUM_CHILDREN,
  };
  enum {
    OBJECT, 
    NUM_OBJECTS,
  };
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  string ToString(bool html) const;
  virtual Function GetFunction() const { return CONSTANT;}

  // ---------- L1 functions ----------  
  void L1_Init() {Expression::L1_Init();}

  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which);

  // ---------- data ----------  

};
struct DynamicConstant : public DynamicExpression {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Object ComputeValue() const;
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};

template <class T> T * MakeStatement() {
  CHECK(static_cast<Statement *>((T *)NULL) == NULL);
  return New<T>();
}
template <class T> T * MakeExpression() {
  CHECK(static_cast<Expression *>((T *)NULL) == NULL);
  return New<T>();
}
template <class T> T * MakeDynamicStatement(OMap binding, 
					    Statement *static_parent) {
  CHECK(static_cast<DynamicStatement *>((T *)NULL) == NULL);
  return New<T>(binding, static_parent);
}
template <class T> T * MakeDynamicExpression(OMap binding, 
					     Statement *static_parent) {
  CHECK(static_cast<DynamicExpression *>((T *)NULL) == NULL);
  return New<T>(binding, static_parent);
}




/*
struct MatchCombineExpression : public Expression {
  Expression *combination_function_
  Expression *pattern_
  Expression *child_
}





struct StaticParallel : public Statement {
  // These have to be implemented differently
  virtual void LinkToChild(Statement * child);
  virtual void UnlinkChild(Statement * child);
  set<Statement *> children_;
};

struct ForEachStatement : public Statement {
  Pattern pattern_;
  Statement *child_;
};

// Match everything with this pattern and choose one (for now) according to 
// the value of a particular variable.  Add the substitution to the working
// substituiton and execute the child statement. 
struct ChooseStatement : public Statement {
  Pattern pattern_;
  Variable likelihood_var_;
  Statement *child_;
};

// Self-modeling chooser object
// Which chooser object to use is determined by run-time evaluation of chooser_
struct BinaryChoice : public Expression {
  Expression * chooser_;  
};

struct FunctionExpression : pubic Expression {
  Keyword function_;
  vector<Expression *> args_;
};

// Returns the sum of var_ over all matches of pattern_
struct SumExpression : public Expression {
  Pattern pattern_;
  Variable var_;
};

// Returns an integer - the number of satisfactions
struct CountExpresison : public Expression { 
  Pattern pattern_;
};

struct RandomBoolExpression : public Expression { 
  Expression * ln_likelihood_;
};
*/



#endif