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

#include "namer.h"

struct Link;

struct Element : public Named {
  Link * parent_;
  virtual bool IsDynamic() const = 0;
  virtual void L1_ConnectToParentLink (Link * link) = 0;
  virtual void L1_DisconnectFromParentLink(Link * link) = 0;
  virtual OMap GetMap() const { CHECK(false); return NULL;}
  Element() :parent_(NULL) {};
  // does not need an L1_Erase, since subclasses' L1_Erase skips it.
};

struct Link {
  Element * parent_;
  Link(Element * parent);
  void L1_Erase();
  Element * GetParent() { return parent_;}
  virtual set<Element *> GetChildren() const = 0;
  virtual void L1_AddChild(Element *child) = 0;
  virtual void L1_RemoveChild(Element *child) = 0;
  virtual ~Link(){};
};
struct MultiLink : public Link {
  MultiLink(Element *parent): Link(parent){}
  map<OMap, Element *> children_;
  Element * GetChild(OMap m);
  void L1_AddChild(Element *child);
  void L1_RemoveChild(Element *child);
  set<Element *> GetChildren() const;
  virtual ~MultiLink(){}
};
struct SingleLink : public Link {
  Element * child_;
  SingleLink(Element *parent) :Link(parent) {}
  Element * GetChild() const { return child_;  }
  void L1_AddChild(Element *child);
  void L1_RemoveChild(Element *child);
  set<Element *> GetChildren() const;
  virtual ~SingleLink(){}
};

/*
Statement{
  SingleLink child_; 
  MultiLink dynamic_;
}
ParallelStatement {
  set<SingleLink *> children_;
}
LetStatement {
   SingleLink * value_; // points to an expression
}
OutputStatement { 
   SingleLink * tuple_;
}
IfStatement  {
   SingleLink * expression_;
   SingleLink * if_true_;
   SingleLink * if_false_;
}

Statement * child_statement = new statemetn... 
parent_statement_->if_true_->L1_AddChild(child_statement);
*/
