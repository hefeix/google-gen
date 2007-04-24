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

struct Statement {  
  virtual ~Statement(){}
};

struct OnStatement : public Statement {
  Pattern pattern_;
  Statement * child_;
};

struct RepeatStatement : 
struct ParallelStatement : public Statement {
  set<Statement *> children_;
};

struct IfStatement : public Statement {
  Expression * expression_;
  Statement * child_;
  Statement * else_;
};

struct DelayStatement : public Statement { 
  EncodedNumber delay_;
  Statement *child_;
};
  
struct LetStatement : public Statement {
  Variable variable_;
  Expression value_;
  Statement *child_;  
};

struct OutputStatement : public Statement {
  Tuple tuple_;
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

struct Expression {  
  virtual ~Expression(){}
};

struct BinaryChoice : public Expression {
  Expression * chooser_;  
};

struct FlakeChoice : public Expression { 
  Expression * chooser_;
};

struct SubstituteExpression : public Expression {
  Expression * arg_;
};

struct ConstantExpression : public Expression { 
  Object object_;
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
  Expression ln_likelihood_;
};

#endif
