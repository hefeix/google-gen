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

#include "base.h"
#include "link.h"
#include "extensions.h"


struct Element : public Base {  
  public:

  #define ElementFunctionList {			\
      ITEM(PASS),					\
	ITEM(ON),					\
	ITEM(REPEAT),					\
	ITEM(DELAY),					\
	ITEM(LET),					\
	ITEM(POST),					\
	ITEM(IF),					\
	ITEM(PARALLEL),					\
	ITEM(SUBSTITUTE),				\
	ITEM(FLAKE_CHOICE),				\
	ITEM(CONSTANT),					\
	ITEM(EQUAL),					\
	ITEM(SUM),					\
	ITEM(FUNCTION_ERROR)				\
	};
  CLASS_ENUM_DECLARE(Element, Function);

  // ---------- L2 functions ----------  
  void SetTime(OTime new_time);
  void ComputeSetTime() { SetTime(ComputeTime()); }

  // ---------- const functions ----------  

  virtual bool IsDynamic() const = 0;
  bool IsStatic() const { return !IsDynamic();}
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
  virtual Element * GetChild(int which) const = 0;
  virtual int NumChildren() const = 0;
  virtual Link::Type LinkType(int which_child) const { return Link::SINGLE;}
  // a human and machine readable version of the static subtree
  virtual string ToString() const = 0;
  virtual string ToStringRecursive(int indent = 0) const {
    return ToString();
  }
  virtual Function GetFunction() const = 0;
  virtual Keyword FunctionKeyword() const { 
    return Keyword::Make(Downcase(FunctionToString(GetFunction())));
  }
  int WhichChildAmI() const { 
    if (parent_) return parent_->WhichChildAmI(); 
    return -1; 
  }
  virtual set<Element *> GetAllChildren() const = 0;
  virtual bool ChildShouldExist(int which_child) const { return true;}
  virtual bool NeedsChildViolation() const;
  
  // ---------- L1 functions ----------  
  Element() :parent_(NULL){};
  virtual void L1_Init() { 
    Base::L1_Init(); 
  }
  void L1_Erase();

  // these functions are only called by the link. 
  // Outsiders should call Link::L1_AddChild() or Link::L1_RemoveChild().
  virtual void L1_ConnectToParentLink (Link * link) = 0;
  virtual void L1_DisconnectFromParentLink(Link * link) = 0;

  void L1_CheckSetChildViolation();
  void L1_CheckSetTimeViolation();
  
  // virtual functions that are here because we need to point to them from the
  // delayed checks.
  virtual void L1_CheckSetParentAndBindingViolations() { CHECK(false);}
  virtual void L1_CheckSetValueViolation() { CHECK(false);}
  virtual void L1_CheckSetLetViolation() { CHECK(false);}
  virtual void L1_CheckSetPostViolation() { CHECK(false);}


  // ---------- N1 notifiers ----------  
  // call this if the stored time or computed time may have changed. 
  // creates a time violation (or destroys one) as necessary. 
  void N1_StoredOrComputedTimeChanged() {
    L1_CheckSetTimeViolation();
  }

  // a child was connected or disconnected.
  virtual void N1_ChildChanged(int which_child);

  virtual void N1_ValueChanged() { CHECK(false);}

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
  bool IsExpressionChild(int which_child) {return 
      (which_child < NumExpressionChildren());}
  virtual int NumChildren() const = 0;
  int NumStatementChildren()  const{ 
    return NumChildren() - NumExpressionChildren(); }
  Base::Type ChildType(int which) {
    CHECK(which>=0 && which < NumChildren());
    return (which < NumExpressionChildren())
      ?Base::EXPRESSION:Base::STATEMENT;
  }
  int NumDynamicChildren() const { return dynamic_children_->children_.size();}
  virtual int NumObjects() const { return 0;}
  // the objects and expression children
  string ParameterListToString() const; 
  // What new variables are introduced by this node
  virtual VariableSet GetIntroducedVariables(int which_child) const;
  // What variables exist at this node. 
  // This is the union of GetIntroducedVariables() up the static tree, 
  // not including this node. 
  VariableSet ComputeVariables() const;
  inline const VariableSet & GetVariables() const { return variables_;}

  Record GetRecordForDisplay() const;
  set<Element *> GetAllChildren() const;
  set<StaticElement *> GetAllStaticChildren() const;
  
  // these convert between the child and object name enums and strings. 
  // they are defined by macros.
  virtual int StringToChild(string s) const = 0;
  virtual string ChildToString(int c) const = 0;
  virtual int StringToObject(string s) const = 0;
  virtual string ObjectToString(int o) const = 0;


  // ---------- L1 functions ----------  
  void L1_Init();
  void L1_Erase();

  void L1_LinkChild(int where, StaticElement *child);
  void L1_UnlinkChild(int where);
  void L1_SetObject(int which, Object new_value);

  // caches the set of variables_ for this and all descendents.
  void L1_RecursivelyComputeSetVariables();

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
  VariableSet variables_;

};

struct DynamicElement : public Element{
  void EraseTree();  

  // ---------- L2 functions ----------  
  bool SetBinding(OMap new_binding); // returns false on failure
  // it can figure out the parent from the static node and the bindings.
  // returns false on failure.
  bool ComputeSetBinding() {
    return SetBinding(ComputeBinding());
  }
  bool LinkToParent(); 
  void UnlinkFromParent();

  // ---------- const functions ----------  
  bool IsDynamic() const { return true; }
  DynamicElement * GetParent() const { 
    if (!parent_) return NULL;
    return (DynamicElement *)(parent_->GetParent());
  }
  Record GetRecordForDisplay() const;
  StaticElement * GetStatic() const {
    if (!static_parent_) return NULL;
    return (StaticElement *)(static_parent_->parent_); 
  }
  string TextIdentifier() const { return binding_.ToString() + " " 
      + (GetStatic()?GetStatic()->ToString():"NO STATIC");}
  int NumExpressionChildren() const { 
    return GetStatic()->NumExpressionChildren();}
  int NumChildren() const { return GetStatic()->NumChildren();}
  int NumStatementChildren() const { 
    return GetStatic()->NumStatementChildren();}
  int NumObjects() const { return GetStatic()->NumObjects();}  
  Object GetObject(int which) const { return GetStatic()->GetObject(which);}
  DynamicElement * GetChild(int which) const {return GetSingleChild(which);}
  DynamicElement * GetSingleChild(int which) const;
  DynamicExpression * GetSingleExpressionChild(int which) const;
  Object GetChildValue(int which) const;
  DynamicStatement * GetSingleStatementChild(int which) const;
  OMap GetBinding() const { return binding_;}
  DynamicElement * FindParent() const; // finds parent based on bindings.
  virtual Link::Type LinkType(int which_child) const { return Link::SINGLE;}
  OTime ComputeTime() const;
  string ToString() const;
  virtual Function GetFunction() const { 
    if (!GetStatic()) return FUNCTION_ERROR;
    return GetStatic()->GetFunction();}
  // this only works for single links.
  virtual OMap GetIntroducedBinding(int which_child) const { 
    return OMap::Default();}
  // this only works for single links now. 
  virtual OMap ComputeChildBinding(int which_child) const {
    CHECK(LinkType(which_child) == Link::SINGLE);
    return OMap::Make(Union(GetBinding().Data(), 
			    GetIntroducedBinding(which_child).Data()));
  }
  // only works for dynamic on statements and children of single links.
  OMap ComputeBinding() {
    if (!GetParent()) return OMap::Default();
    return GetParent()->ComputeChildBinding(WhichChildAmI());
  }
  // static parent and binding need to exist. binding needs to be correct
  // when restricted to the variables of the parent of the static parent. 
  Link * FindDynamicParentLink() const;
  set<Element *> GetAllChildren() const;

  // ---------- L1 functions ----------  
  // we probably only need two of these parameters, since we can figure out 
  // the third, but we'd rather pass all three here than worry about the corner
  // cases at this point. 
  void L1_Init(StaticElement * static_parent, OMap binding);
  void L1_Erase();
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
  virtual void N1_ChildExpressionValueChanged(int which_child) = 0;

  // The following is called externally when a child is connected to
  // or disconnected from this parent node. 
  void N1_ChildChanged(int which_child);

  // called when the binding changes
  virtual void N1_BindingChanged();

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
  Base::Type GetBaseType() const { return Base::STATEMENT; }
  string ToStringRecursive(int indent) const; // includes the subtree
  string ToString() const;
  string TextIdentifier() const { return ToString();}

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
  //static Expression * MakeExpression(Keyword type);
  // ---------- const functions ----------  
  string ToString() const;
  Base::Type GetBaseType() const { return Base::EXPRESSION;}
  virtual int NumExpressionChildren() const { return NumChildren();}
  virtual int NumChildren() const = 0;
  virtual int NumObjects() const = 0;
  string TextIdentifier() const { return ToString();}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};




struct DynamicStatement : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Base::Type GetBaseType() const { return Base::DYNAMIC_STATEMENT;}
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
  void ComputeSetValue(){SetValue(ComputeValue());}
  // ---------- const functions ----------  
  Record GetRecordForDisplay() const;
  virtual Object ComputeValue() const = 0;
  Object GetValue() const { return value_;}
  Base::Type GetBaseType() const { return Base::DYNAMIC_EXPRESSION;}
  string TextIdentifier() const { 
    return DynamicElement::TextIdentifier() + " value=" + value_.ToString();
  }
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding);
  void L1_Erase();
  void L1_CheckSetValueViolation();

  // ---------- N1 Notifiers ----------
  // TODO: forward this
  void N1_ChildExpressionValueChanged(int which_child) { 
    L1_CheckSetValueViolation();}
  void N1_ValueChanged() { L1_CheckSetValueViolation();}  // checks whether the value is correct, and creates/removes a violation.

  // ---------- data ----------  
  Object value_;
};

#define DECLARE_FUNCTION_ENUMS \
  int StringToChild(string s) const{ return StringToChildName(s);} \
  string ChildToString(int c) const{ return ChildNameToString((ChildName)c);} \
  int StringToObject(string s) const{ return StringToObjectName(s);} \
  string ObjectToString(int o) const{return ObjectNameToString((ObjectName)o);}\
  int NumObjects() const { return NumObjectNames();} \
  int NumChildren() const { return NumChildNames();}

struct StaticPass : public Statement {
  #define StaticPassChildNameList {				\
	};
  CLASS_ENUM_DECLARE(StaticPass, ChildName);
  #define StaticPassObjectNameList {				\
	};
  CLASS_ENUM_DECLARE(StaticPass, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return 0;}
  virtual Function GetFunction() const { return PASS;}
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};
struct DynamicPass : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  void N1_ChildExpressionValueChanged(int which_child) { CHECK(false);}
  // ---------- data ----------  
};

struct StaticOn : public Statement {
  #define StaticOnChildNameList {				\
      ITEM(CHILD),					\
	};
  CLASS_ENUM_DECLARE(StaticOn, ChildName);
  #define StaticOnObjectNameList {				\
      ITEM(PATTERN),					\
	};
  CLASS_ENUM_DECLARE(StaticOn, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  OPattern GetPattern() const { return GetObject(PATTERN);}
  virtual Function GetFunction() const { return ON;}
  VariableSet GetIntroducedVariables(int which_child) const;
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
  OnMultiLink * GetOnMultilink() const { 
    return dynamic_cast<OnMultiLink *>(children_[StaticOn::CHILD]);
  }
  OPattern GetPattern() const { return GetStaticOn()->GetPattern();}
  OTime ComputeChildTime(const Link * link, const Element *child) const;
  Record GetRecordForDisplay() const;
  OTime ComputeTime() const;
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement* parent, OMap dummy);
  void L1_Erase();
  // --------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) { CHECK(false); }
  // ---------- data ----------  
};


struct StaticRepeat : public Statement {
  #define StaticRepeatChildNameList {				\
      ITEM(REPETITIONS),					\
      ITEM(CHILD),					\
	};
  CLASS_ENUM_DECLARE(StaticRepeat, ChildName);
  #define StaticRepeatObjectNameList {				\
      ITEM(REPETITION_VARIABLE),					\
	};
  CLASS_ENUM_DECLARE(StaticRepeat, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  Variable GetRepetitionVariable() const { 
    return GetObject(REPETITION_VARIABLE);}
  virtual Function GetFunction() const { return REPEAT;}
  VariableSet GetIntroducedVariables(int which_child) const {
    if (which_child == CHILD) 
      return Singleton<VariableSet>(GetRepetitionVariable());
    return VariableSet();
  }
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
  void N1_ChildExpressionValueChanged(int which_child) { 
    // todo: check whether the number of repetitions is correct
    CHECK(false);
  }
  // ---------- data ----------  
};


struct StaticDelay : public Statement { 
  #define StaticDelayChildNameList {				\
      ITEM(DIMENSION),					\
      ITEM(CHILD),					\
	};
  CLASS_ENUM_DECLARE(StaticDelay, ChildName);
  #define StaticDelayObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticDelay, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
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
      if (time_ == NULL) return OTime();
      DynamicExpression *delay_expression 
	= GetSingleExpressionChild(StaticDelay::DIMENSION);
      if (!delay_expression) return OTime();
      Object dimension = delay_expression->value_;
      if (dimension.GetType() != Object::OBITSEQ) return OTime();
      return OTime::Make
	(time_.Data() 
	 + OBitSeq(dimension).Data());
    }
    return time_;
  }
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) { 
    // todo: check whether the times are right on the children
    DynamicStatement * child = GetSingleStatementChild(StaticDelay::CHILD);
    if (child) child->N1_StoredOrComputedTimeChanged();
  }
  // ---------- data ----------  
};
  
struct StaticLet : public Statement {
  #define StaticLetChildNameList {				\
      ITEM(VALUE),					\
      ITEM(CHILD),					\
	};
  CLASS_ENUM_DECLARE(StaticLet, ChildName);
  #define StaticLetObjectNameList {				\
      ITEM(VARIABLE),					\
	};
  CLASS_ENUM_DECLARE(StaticLet, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return CHILD;}
  Variable GetVariable() const { return GetObject(VARIABLE);}
  virtual Function GetFunction() const { return LET;}

  VariableSet GetIntroducedVariables(int which_child) const {
    if (which_child == CHILD) return Singleton<VariableSet>(GetVariable());
    return VariableSet();
  }
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){CHECK(NumDynamicChildren() == 0);}
  // ---------- data ----------  
};
struct DynamicLet : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  StaticLet *GetStaticLet() const { 
    return dynamic_cast<StaticLet*>(GetStatic());
  }
  bool NeedsLetViolation() const; // a let violation should exist.
  OMap GetIntroducedBinding(int which_child) const { 
    if (which_child != StaticLet::CHILD) return OMap::Default();
    Map ret;
    ret[GetStaticLet()->GetVariable()] = GetChildValue(StaticLet::VALUE);
    return OMap::Make(ret);
  }
  // ---------- L1 functions ----------  
  void L1_CheckSetLetViolation(); // (programming with Dr. Seuss)
  void L1_Init(StaticElement * static_parent, OMap binding) {
    DynamicStatement::L1_Init(static_parent, binding);
    L1_CheckSetLetViolation();
  }
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) { 
    L1_CheckSetLetViolation();
  }
  // ---------- data ----------  
};

struct StaticPost : public Statement {
  #define StaticPostChildNameList {				\
      ITEM(TUPLE),					\
	};
  CLASS_ENUM_DECLARE(StaticPost, ChildName);
  #define StaticPostObjectNameList {				\
	};
  CLASS_ENUM_DECLARE(StaticPost, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return NumChildren();}
  virtual Function GetFunction() const { return POST;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};
struct DynamicPost : public DynamicStatement {
  // ---------- L2 functions ----------  
  void AddCorrectPosting();
  void AddPosting(OTuple t, OTime time);
  void RemovePosting();
  void SetPostingTime(OTime new_time);
  void SetCorrectPostingTime() { SetPostingTime(time_);}
  // ---------- const functions ----------  
  bool NeedsPostViolation() const;
  DynamicExpression * GetTupleExpression() const {
    return dynamic_cast<DynamicExpression *>
      (GetSingleChild(StaticPost::TUPLE));
  }  
  Object ComputeTuple() const{
    DynamicExpression * expr = GetTupleExpression();
    if (!expr) return Object();
    return expr->GetValue();
  }
  OwnedPosting * GetOwnedPosting() const { return posting_;}
  Record GetRecordForDisplay() const;
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding) {
    posting_ = NULL;
    DynamicStatement::L1_Init(static_parent, binding);
    L1_CheckSetPostViolation();
  }
  // see if it's perfect, then create or remove violation if necessary.
  void L1_CheckSetPostViolation();
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) { 
    L1_CheckSetPostViolation();
  }  
  // ---------- data ----------  
  OwnedPosting * posting_;
};

// if the condition does not evaluate to Boolean::Make(false), 
// then the ON_TRUE should be executed.
struct StaticIf : public Statement {
  #define StaticIfChildNameList {					\
      ITEM(CONDITION),							\
	ITEM(ON_TRUE),					\
	ITEM(ON_FALSE),					\
	};
  CLASS_ENUM_DECLARE(StaticIf, ChildName);
  #define StaticIfObjectNameList {				\
	};
  CLASS_ENUM_DECLARE(StaticIf, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return ON_TRUE;}
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
  bool NeedsIfViolation() const;
  StaticIf *GetStaticIf() const { 
    return dynamic_cast<StaticIf*>(GetStatic());
  }
  bool ChildShouldExist(int which_child) const;


  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding) {
    DynamicStatement::L1_Init(static_parent, binding);
  }
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) {
    L1_CheckSetChildViolation();
  }
  // ---------- data ----------  
};


struct StaticParallel : public Statement {
  #define StaticParallelChildNameList {					\
	};
  CLASS_ENUM_DECLARE(StaticParallel, ChildName);
  #define StaticParallelObjectNameList {				\
	};
  CLASS_ENUM_DECLARE(StaticParallel, ObjectName);
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  int NumExpressionChildren() const { return 0;}
  int NumChildren() const { return static_children_.size(); }
  int StringToChild(string s) const { CHECK(false); return 0;}
  string ChildToString(int i) const { return "CHILD(" + itoa(i) + ")";}
  int StringToObject(string s) const { CHECK(false); return 0;}
  string ObjectToString(int i) const { CHECK(false); return "";}

  virtual Function GetFunction() const { return PARALLEL;}

  // ---------- L1 functions ----------  
  // ---------- data ----------  
};

struct DynamicParallel : public DynamicStatement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildExpressionValueChanged(int which_child) { CHECK(false); }
  // ---------- data ----------  
};


struct StaticSubstitute : public Expression {
   #define StaticSubstituteChildNameList {	 		\
    ITEM(CHILD),						\
      };
  CLASS_ENUM_DECLARE(StaticSubstitute, ChildName);
  #define StaticSubstituteObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticSubstitute, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  virtual Function GetFunction() const { return SUBSTITUTE;}
};
struct DynamicSubstitute : public DynamicExpression {
  Object ComputeValue() const;
  void N1_BindingChanged() {
    DynamicElement::N1_BindingChanged();
    L1_CheckSetValueViolation();
  }
};

struct StaticEqual : public Expression {
   #define StaticEqualChildNameList {	 		\
    ITEM(LHS),						\
    ITEM(RHS),						\
      };
  CLASS_ENUM_DECLARE(StaticEqual, ChildName);
  #define StaticEqualObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticEqual, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return EQUAL;}
};
struct DynamicEqual : public DynamicExpression {
  Object ComputeValue() const;
};
struct StaticSum : public Expression {
   #define StaticSumChildNameList {	 		\
    ITEM(LHS),						\
    ITEM(RHS),						\
      };
  CLASS_ENUM_DECLARE(StaticSum, ChildName);
  #define StaticSumObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticSum, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return SUM;}
};
struct DynamicSum : public DynamicExpression {
  Object ComputeValue() const;
};


struct StaticFlakeChoice : public Expression { 
   #define StaticFlakeChoiceChildNameList {	 		\
    ITEM(CHOOSER),						\
      };
  CLASS_ENUM_DECLARE(StaticFlakeChoice, ChildName);
  #define StaticFlakeChoiceObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(StaticFlakeChoice, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return FLAKE_CHOICE;}
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
   #define StaticConstantChildNameList {	 		\
      };
  CLASS_ENUM_DECLARE(StaticConstant, ChildName);
  #define StaticConstantObjectNameList {		\
    ITEM(OBJECT),						\
    };
  CLASS_ENUM_DECLARE(StaticConstant, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  string ToString() const;
  virtual Function GetFunction() const { return CONSTANT;}

  // ---------- L1 functions ----------  
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
template <class T> T * MakeDynamicElement(StaticElement *static_parent, 
					  OMap binding) {
  CHECK(static_cast<DynamicElement *>((T *)NULL) == NULL);
  return New<T>(static_parent, binding);
}

DynamicElement * MakeDynamicElement(StaticElement *static_parent, 
				    OMap binding);




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

#define ALL_FUNCTIONS \
  FUNCTION(Pass, PASS)				\
       FUNCTION(On, ON)				\
       FUNCTION(Repeat, REPEAT)			\
       FUNCTION(Delay, DELAY)			\
       FUNCTION(Let, LET)			\
       FUNCTION(Post, POST)			\
       FUNCTION(If, IF)				\
       FUNCTION(Parallel, PARALLEL)				\
       FUNCTION(Substitute, SUBSTITUTE)				\
       FUNCTION(FlakeChoice, FLAKE_CHOICE)				\
       FUNCTION(Constant, CONSTANT)				\
       FUNCTION(Equal, EQUAL)				\
       FUNCTION(Sum, SUM)				\
 

    

#endif
