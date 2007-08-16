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

#include "namer.h"
#include "link.h"



/*struct Link;
struct MultiLink;
struct SingleLink;*/

struct Element : public Named {
  Link * parent_;
  virtual bool IsDynamic() const = 0;
  virtual void L1_ConnectToParentLink (Link * link) = 0;
  virtual void L1_DisconnectFromParentLink(Link * link) = 0;
  virtual OMap GetBinding() const { CHECK(false); return OMap();}
  virtual void Init() { Named::Init(); }
  Element() :parent_(NULL){};
  OTime time_;
  virtual OTime ComputeChildTime(Link * link, Element *child) {
    return time_;
  }
  // does not need an L1_Erase, since subclasses' L1_Erase skips it.
};

struct Statement;
struct Expression;

struct StaticElement : public Element {
  void Init();
  bool IsDynamic() const { return false; }
  void L1_ConnectToParentLink(Link *link) { parent_ = link;}
  void L1_DisconnectFromParentLink(Link *link) { 
    CHECK(parent_ == link);
    parent_ = NULL;
  }
  void L1_Erase();

  // these links (like all links) are owned by the parent
  MultiLink * dynamic_children_;
  vector<SingleLink *> static_children_; // statements and expressions
  vector<Object> objects_;

  StaticElement * GetParent() const;
  StaticElement * GetChild(int which) const;
  Statement * GetStatementChild(int which) const;// which < NumStatementChildren
  Expression * GetExpressionChild(int which) const;
  vector<Statement *> GetStatementChildren() const;
  void L1_LinkChild(int where, StaticElement *child);
  void L1_UnlinkChild(int where);
  Object GetObject(int which) const;
  void L1_SetObject(int which, Object new_value);

  virtual int NumExpressionChildren() const = 0;
  virtual int NumChildren() const = 0;
  virtual int NumStatementChildren()  const{ 
    return NumChildren() - NumExpressionChildren(); }
  virtual int NumObjects() const { return 0;}
  virtual Keyword TypeKeyword() const = 0;
  string ParameterListToString() const; // the objects and expression children
  // What new variables are introduced by this node
  virtual set<Variable> GetIntroducedVariables() const;
  // What variables exist at this node. 
  // This is the union of GetIntroducedVariables() up the static tree, 
  // not including this node. 
  virtual set<Variable> GetVariables() const;
};

struct DynamicElement : public Element{
  bool IsDynamic() const { return true; }
  StaticElement * GetParent() const { 
    if (!parent_) return NULL;
    return dynamic_cast<StaticElement *>(parent_->GetParent());
  }
  void L1_ConnectToParentLink(Link *link) {
    if (link->parent_->IsDynamic())
      parent_ = link;
    else static_parent_ = link;
  }
  StaticElement * GetStatic() const {
    return dynamic_cast<StaticElement *>(static_parent_->parent_); 
  }
  int NumExpressionChildren() { return GetStatic()->NumExpressionChildren();}
  int NumChildren() { return GetStatic()->NumChildren();}
  int NumStatementChildren() { return GetStatic()->NumStatementChildren();}
  int NumObjects() { return GetStatic()->NumObjects();}  
  Object GetObject(int which) const{ return GetStatic()->GetObject(which);}
  DynamicElement * GetSingleChild(int which) const;
  OMap GetBinding() const { return binding_;}
  DynamicElement * FindParent() const; // finds parent based on bindings.
  virtual Link::Type LinkType(int which_child) { return Link::SINGLE;}
  // this gets called when the value of a child expression changes, 
  // or when a child expresison is created(Init) or destroyed(L1_Erase). 
  virtual void ChildExpressionChanged();

  OMap ComputeBinding() const;
  OTime ComputeTime() const;

  virtual OMap ChildBinding() const;
  Link * static_parent_;
  vector<Link *> children_;
  OMap binding_; // if parent_ is a multilink, always matches it. 
  // we probably only need two of these parameters, since we can figure out 
  // the third, but we'd rather pass all three here than worry about the corner
  // cases at this point. 
  void Init(Link * static_parent, Link *parent, OMap binding);
  void L1_Erase() { Element::L1_Erase();}
  // TimeViolation * time_violation_; TODO
};

struct Statement : public StaticElement{
  
  // Constructors etc.
  void Init(); // shadow constructor
  virtual ~Statement(){}
  Named::Type GetType() const { return Named::STATEMENT; }
  
  // Erasing. Can only erase unlinked statements.
  virtual void L1_Erase();
  
  // position points to where to start parsing, and is changed by the function
  // to the end of what was parsed.
  static Statement * ParseSingle(const Tuple & t, uint * position);
  static vector<Statement *> Parse(const Tuple & t); // ad hoc parser.
  string ToString(int indent) const; // includes the subtree
  string ToStringSingle() const;
  static Statement * MakeStatement(Keyword type);

  protected:
};

struct Expression : public StaticElement {  
  void Init();
  static Expression * Parse(const Object & o); // ad hoc parser.
  virtual string ToString() const;
  Named::Type GetType() const { return Named::EXPRESSION;}
  virtual int NumExpressionChildren() const { return NumChildren();}
  virtual int NumChildren() const = 0;
  virtual int NumObjects() const { return 0;}
  static Expression * MakeExpression(Keyword type);
};




struct DynamicStatement : public DynamicElement {
  void L1_Erase() { DynamicElement::L1_Erase();}
  void Init(Link * static_parent, Link *parent, OMap binding){
    DynamicElement::Init(static_parent, parent, binding);
  }
};

struct DynamicExpression : public DynamicElement {
  void Init(Link * static_parent, Link *parent, OMap binding);
  void L1_Erase();
  virtual Object ComputeValue() const;
  void ChildExpressionChanged() { CheckSetValueViolation();}
  void SetValue(Object new_value);
  // checks whether the value is correct, and creates/removes a violation.
  void CheckSetValueViolation();
  Object value_;
};


struct PassStatement : public Statement {
  int NumExpressionChildren() const { return 0;}
  int NumChildren() const { return 0;}
  int NumObjects() const { return 0;}
  Keyword TypeKeyword() const { return Keyword::Make("pass");}
  struct Dynamic : public DynamicStatement {
  };
};

struct OnStatement : public Statement {
  enum {
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    PATTERN, // OPattern
    NUM_OBJECTS,
  };
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  OPattern GetPattern() const { return GetObject(PATTERN);}
  Keyword TypeKeyword() const;
  set<Variable> GetIntroducedVariables() const;
  
  void Init();
  void L1_Erase();
  void L1_Subscribe(); // subscribe to the appropriate query.
  SubType * subscription_;

  struct Dynamic : public DynamicStatement {
    void Init(OnStatement *static_parent);
    virtual bool LinkType(int which_child) { return Link::ON;}
    void GetPattern() { return GetStatic()->GetPattern();}
  };
};

struct RepeatStatement : public Statement {
  enum {
    REPETITIONS,
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    REPETITION_VARIABLE,
    NUM_OBJECTS,
  };
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  Variable GetRepetitionVariable() { return GetObject(REPETITION_VARIABLE);}
  Keyword TypeKeyword() const;
  set<Variable> GetIntroducedVariables() const {
    return SingletonSet(GetRepetitionVariable()); }

  void Init();
  // this variable is useless except to preserve the property that a dynamic
  // node is associated with a unique (static node, substitution) pair.
  // assigned automatically in the constructor.

  struct Dynamic : public DynamicStatement {
    virtual bool LinkType(int which_child) { 
      return (which_child==CHILD)?Link::MULTI:Link::SINGLE;
    }
  };
};

struct DelayStatement : public Statement { 
  enum {
    DELAY,
    CHILD,
    NUM_CHILDREN,
  };
  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  
  void Init();

  struct Dynamic : public DynamicStatement {
    virtual OTime ComputeChildTime(Link * link, Element *child) {
      if (link_ == children_[CHILD]) {
	return OTime::Make(time_.Data() + GetSingleChild(DELAY)->value_);
      }
      return time_;
    }
  };
};
  
struct LetStatement : public Statement {
  enum {
    VALUE,
    CHILD,
    NUM_CHILDREN,
  };
  enum {
    VARIABLE,
    NUM_OBJECTS,
  };

  int NumExpressionChildren() const { return CHILD;}
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  Variable GetVariable() { return GetObject(VARIABLE);}
  Keyword TypeKeyword() const;
  set<Variable> GetIntroducedVariables() const {
    return SingletonSet(GetVariable());}
  void Init();

  struct Dynamic : public DynamicStatement {
  };
};

struct OutputStatement : public Statement {
  enum {
    TUPLE,
    NUM_CHILDREN,
  };
  int NumExpressionChildren() const { return NUM_CHILDREN;}
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init();

  struct Dynamic : public DynamicStatement {
    bool IsPerfect() const;
    // see if it's perfect, then create or remove violation if necessary.
    void CheckSetPostingViolation();
    void ChildExpressionChanged() { CheckSetPostingViolation();}    
    DynamicExpression * GetTupleExpression() const {
      return dynamic_cast<DynamicExpression *>(GetSingleChild(TUPLE));
    }
    
    Posting * posting_;
  };
};
typedef OwnedViolation<OutputStatement::Dynamic, Violation::POSTING>
  PostingViolation;

struct IfStatement : public Statement {
  enum {
    CONDITION,
    IF,
    ELSE,
    NUM_CHILDREN,
  };
  int NumExpressionChildren() const { return IF;}
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init();

  struct Dynamic : public DynamicStatement {
  };
};

struct ParallelStatement : public Statement {
  int NumExpressionChildren() const { return 0;}
  int NumChildren() const { return static_children_.size(); }
  Keyword TypeKeyword() const { return Keyword::Make("parallel");}
  void Init() { Statement::Init();}

  struct Dynamic : public DynamicStatement {
  };
};


struct SubstituteExpression : public Expression {
  enum {
    CHILD,
    NUM_CHILDREN,
  };
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init() {Expression::Init();}

  struct Dynamic : public DymamicExpression() {
  };
};

struct FlakeChoiceExpression : public Expression { 
  enum {
    CHOOSER, // if the chooser is null, use the global flake chooser
    NUM_CHILDREN,
  };
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init() {Expression::Init();}

  struct Dynamic : public DymamicExpression() {
  };
};

struct ConstantExpression : public Expression {
  enum {
    NUM_CHILDREN,
  };
  enum {
    OBJECT, 
    NUM_OBJECTS,
  };
  int NumChildren() const { return NUM_CHILDREN;}
  int NumObjects() const { return NUM_OBJECTS;}
  void Init() {Expression::Init();}
  Keyword TypeKeyword() const;
  string ToString() const;

  struct Dynamic : public DymamicExpression() {
  };
};


/*
struct MatchCombineExpression : public Expression {
  Expression *combination_function_
  Expression *pattern_
  Expression *child_
}





struct ParallelStatement : public Statement {
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
