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

class Statement : public Named{
 public:
  
  // Constructors etc.
  Statement();
  virtual ~Statement(){}
  NamedType Type() const { return STATEMENT; }
  
  // Erasing. Can only erase unlinked statements.
  virtual void L1_Erase();
  
  // child_ is only used if you can only have 1 child
  Statement * child_;
  Statement * parent_;

  virtual uint32 GetNumChildren() {
    if (child_) return 1;
    return 0;
  }
  virtual vector<Statement *> GetChildren() const {
    vector<Statement *> ret;
    if (child_) ret.push_back(child_);
    return ret;
  }
  
  // Hook up static nodes to each other
  void ConnectToParent(Statement * parent);
  void DisconnectFromParent();

  // position points to where to start parsing, and is changed by the function
  // to the end of what was parsed.
  static Statement * ParseSingle(const Tuple & t, uint * position);
  static vector<Statement *> Parse(const Tuple & t); // ad hoc parser.
  string ToString(int indent) const;
  virtual string ToStringSingle() const = 0;

 protected:
  virtual void L1_LinkToChild(Statement * child);
  virtual void L1_UnlinkChild(Statement * child);
  
  map<Map, DynamicStatement*> dynamic_statements_;


};


struct OnStatement : public Statement {
  typedef UpdateSubscription<QueryUpdate, Query, OnStatement> SubType;
  friend class UpdateSubscription<QueryUpdate, Query, OnStatement>;
  //friend class SubType;

  OnStatement(OPattern p);
  string ToStringSingle() const;
  
  OPattern pattern_;
  SubType * subscription_;
};

struct RepeatStatement : public Statement {
  RepeatStatement(Expression * number_of_repetitions,
		  Variable repetition_name);
  string ToStringSingle() const;

  Expression * number_of_repetitions_;
  // this variable is useless except to preserve the property that a dynamic
  // node is associated with a unique (static node, substitution) pair.
  Variable repetition_name_; 
};

struct DelayStatement : public Statement { 
  DelayStatement(OBitSeq delay);
  string ToStringSingle() const;
  OBitSeq delay_;
};
  
struct LetStatement : public Statement {
  LetStatement(Variable variable, 
	       Expression * value);
  string ToStringSingle() const;
  Variable variable_;
  Expression * value_;
};

struct OutputStatement : public Statement {
  OutputStatement(OTuple tuple);
  string ToStringSingle() const;

  OTuple tuple_;

  // no children
  void L1_LinkToChild(Statement * child) { CHECK(false); }
};

struct Expression : public Named{  
  Expression();
  static Expression * Parse(const Tuple & t); // ad hoc parser.
  virtual string ToString() const = 0;
  NamedType Type() const { return EXPRESSION;}
};

struct SubstituteExpression : public Expression {
  SubstituteExpression(Object object);
  string ToString() const;
  Object object_;
};

struct FlakeChoice : public Expression { 
  FlakeChoice(Expression *);
  string ToString() const;

  // if the chooser is null, uses the global flake chooser.
  Expression * chooser_;
};




/*
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
