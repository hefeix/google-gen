#ifndef _DYNAMIC_H_
#define _DYNAMIC_H_

#include "static.h"

struct DynamicStatement : public Named {
  Statement * statement_;
  DynamicStatement * parent_;
  Map sub_;
  Time time_;
};

struct DynamicExpression : public Named {
  Expression * expression_;
  Map sub_;
  Time time_;
  Object value_;
};

struct DynamicOnStatement : public DynamicStatement {
  map<Map, DynamicStatement*> children_;
}

struct DynamicRepeatStatement : public DynamicStatement {
  map<Object, DynamicStatement*> children_;
  DynamicExpression * number_of_repetitions_;
};

struct DynamicDelayStatement : public DynamicStatement {
  DynamicStatement * child_;
};

struct DynamicLetStatement : public DynamicStatement {
  DynamicExpression * value_;
  DynamicStatement * child_;
};

struct DynamicOutputStatement : public DynamicStatement {
  Posting * posting_;
};

#endif
