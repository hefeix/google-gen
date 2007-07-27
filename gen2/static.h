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

#ifndef _STATIC_H_
#define _STATIC_H_

#include "objects.h"
#include "namer.h"
#include "query.h"
#include "element.h"

class Statement;
class Expression;
class DynamicStatement;
class DynamicExpression;

struct StaticElement : public Element {
  void Init();
  bool IsDynamic() const { return false; }
  void L1_ConnectToParentLink(Link *link) { parent_ = link;}
  void L1_DisconnectFromParentLink(Link *link) { 
    CHECK(parent_ == link);
    parent_ = NULL;
  }
  void L1_Erase();

  MultiLink * dynamic_children_;
  vector<SingleLink *> static_children_; // statements and expressions
  vector<Object> objects_;

  Element * GetChild(int which) const;
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
  
  typedef UpdateSubscription<QueryUpdate, Query, OnStatement> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, OnStatement>;
  //friend class SubType;
  void Update(const QueryUpdate &update, SubType *sub);

  void Init();
  void L1_Subscribe(); // subscribe to the appropriate query.
  SubType * subscription_;
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

  void Init();
  // this variable is useless except to preserve the property that a dynamic
  // node is associated with a unique (static node, substitution) pair.
  // assigned automatically in the constructor.
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

  void Init();
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
};

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

struct SubstituteExpression : public Expression {
  enum {
    CHILD,
    NUM_CHILDREN,
  };
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init() {Expression::Init();}
};

struct FlakeChoiceExpression : public Expression { 
  enum {
    CHOOSER, // if the chooser is null, use the global flake chooser
    NUM_CHILDREN,
  };
  int NumChildren() const { return NUM_CHILDREN;}
  Keyword TypeKeyword() const;
  void Init() {Expression::Init();}
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
