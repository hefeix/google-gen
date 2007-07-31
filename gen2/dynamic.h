/ Copyright (C) 2007 Google Inc. and Georges Harik
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
#ifndef _DYNAMIC_H_
#define _DYNAMIC_H_

#include "static.h"

struct DynamicElement : public Element{
  bool IsDynamic() const { return true; }
  void L1_ConnectToParentLink(Link *link) {
    if (link->parent_->IsDynamic())
      parent_ = link;
    else static_ = link;
  }
  Link * static_parent_;
  Map sub_;
  Time time_;
};

struct DynamicStatement : public DynamicElement {
  Statement * GetStatic() {
    return dynamic_cast<Statement *>(static_parent_->parent_);
  }
  DynamicStatement * parent_;
};

struct DynamicExpression : public DynamicElement {
  Expression * GetStatic() {
    return dynamic_cast<Expression *>(static_parent_->parent_);
  }
  Object value_;
};

struct DynamicOnStatement : public DynamicStatement {
  DynamicOnStatement() :children_(this) {}
  MultiLink children_;
}

struct DynamicRepeatStatement : public DynamicStatement {
  DynamicRepeatStatement() :children_(this), number_of_repetitions_(this) {}  
  DynamicExpression * GetNumberOfRepetitions() {
    return dynamic_cast<DynamicExpression *>(number_of_repetitions_);
  }
  MultiLink children_;
  SingleLink number_of_repetitions_;
};

struct DynamicDelayStatement : public DynamicStatement {
  DynamicDelayStatement() :child_(this){}
  DynamicStatement * GetChild() {
    return dynamic_cast<DynamicStatement *>(child_);
  }
  SingleLink child_;
};

struct DynamicLetStatement : public DynamicStatement {
  DynamicLetStatement() :value_(this), child_(this) {}
  DynamicExpression * GetValue() {
    return dynamic_cast<DynamicExpression *>(value_);
  }
  DynamicStatement * GetChild() {
    return dynamic_cast<DynamicStatement *>(child_);
  }
  SingleLink value_;
  SingleLink child_;
};

struct DynamicOutputStatement : public DynamicStatement {
  DynamicOutputStatement : tuple_(this), posting_(NULL) {}
  DynamicExpression * GetTuple() {
    return dynamic_cast<DynamicExpression *>(tuple_);
  }
  SingleLink tuple_;
  Posting * posting_;
};

#endif
