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

class Statement;
class Expression;
class DynamicStatement;
class DynamicExpression;

struct StaticElement : public Element{
  StaticElement();
  bool IsDynamic() const { return false; }
  void L1_ConnectToParentLink(Link *link) { parent_ = link;}
  void L1_Erase();

  MultiLink * dynamic_children_;
  vector<SingleLink *> static_children_; // statements and expressions
  vector<Object> objects_;

  Element * GetChild(int which);
  Statement * GetStatementChild(int which);
  Expression * GetExpressionChild(int which);
  void CreateChildren(int num);
  void L1_LinkChild(int where, StaticElement *child);
  void L1_UnlinkChild(int where);
  Object GetObject(int which);
  void L1_SetObject(int which, Object new_value);

  virtual int NumExpressionChildren() = 0;
  virtual int NumChildren() = 0;
  virtual int NumStatementChildren() { 
    return NumChildren() - NumExpressionChildren(); }  
};

struct Statement : public StaticElement{
  
  // Constructors etc.
  Statement();
  virtual ~Statement(){}
  NamedType Type() const { return STATEMENT; }
  
  // Erasing. Can only erase unlinked statements.
  virtual void L1_Erase();
  
  // Hook up static nodes to each other
  void ConnectToParent(Statement * parent);
  void DisconnectFromParent();

  // position points to where to start parsing, and is changed by the function
  // to the end of what was parsed.
  static Statement * ParseSingle(const Tuple & t, uint * position);
  static vector<Statement *> Parse(const Tuple & t); // ad hoc parser.
  string ToString(int indent) const;
  virtual string ToStringSingle() const = 0;
  virtual int NumExpressionChildren() = 0;
  virtual int NumChildren() = 0;
  virtual int NumObjects() { return 0;}

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
  int NumExpressionChildren() { return CHILD;}
  int NumChildren() { return NUM_CHILDREN;}
  int NumObjects() { return NUM_OBJECTS;}
  OPattern GetPattern() const { return GetObject(PATTERN);}
  
  typedef UpdateSubscription<QueryUpdate, Query, OnStatement> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, OnStatement>;
  //friend class SubType;
  void Update(const QueryUpdate &update, SubType *sub);

  OnStatement();
  string ToStringSingle() const;
  
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
  }
  int NumExpressionChildren() { return CHILD;}
  int NumChildren() { return NUM_CHILDREN;}
  int NumObjects() { return NUM_OBJECTS;}
  Variable GetRepetitionVariable() { return GetObject(REPETITION_VARIABLE);}

  RepeatStatement();
  string ToStringSingle() const;
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
  int NumExpressionChildren() { return CHILD;}
  int NumChildren() { return NUM_CHILDREN;}
  
  DelayStatement();
  string ToStringSingle() const;
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
  }

  int NumExpressionChildren() { return CHILD;}
  int NumChildren() { return NUM_CHILDREN;}
  int NumObjects() { return NUM_OBJECTS;}
  Variable GetVariable() { return GetObject(VARIABLE);}

  LetStatement();
  string ToStringSingle() const;
};

struct OutputStatement : public Statement {
  enum {
    TUPLE,
    NUM_CHILDREN,
  };
  int NumExpressionChildren() { return NUM_CHILDREN;}
  int NumChildren() { return NUM_CHILDREN;}

  OutputStatement();
  string ToStringSingle() const;
};

struct Expression : public StaticElement {  
  Expression();
  static Expression * Parse(const Tuple & t); // ad hoc parser.
  virtual string ToString() const = 0;
  NamedType Type() const { return EXPRESSION;}
  virtual int NumExpressionChildren() { return NumChildren();}
  virtual int NumChildren() = 0;
  virtual int NumObjects() { return 0;}
};

struct SubstituteExpression : public Expression {
  enum {
    CHILD,
    NUM_CHILDREN,
  };
  int NumChildren() { return NUM_CHILDREN;}
  SubstituteExpression();
  string ToString() const;
};

struct FlakeChoice : public Expression { 
  enum {
    CHOOSER, // if the chooser is null, use the global flake chooser
    NUM_CHILDREN,
  };
  int NumChildren() { return NUM_CHILDREN;}
  FlakeChoice();
  string ToString() const;
};

struct ConstantExpression : public Expression {
  enum {
    NUM_CHILDREN,
  };
  enum {
    OBJECT, 
    NUM_OBJECTS,
  }
  int NumChildren() { return NUM_CHILDREN;}
  int NumObjects() { return NUM_OBJECTS;}
  ConstantExpression();
  string ToString() const;
};

/*
struct MatchCombineExpression : public Expression {
  Expression *combination_function_
  Expression *pattern_
  Expression *child_
}





struct IfStatement : public Statement {
  Expression * expression_;
  Statement * child_;
  Statement * else_;
};

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
