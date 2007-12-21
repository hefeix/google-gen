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
#include "chooser.h"

#define ALL_FUNCTIONS \
  FUNCTION(On, ON)				\
       FUNCTION(Match, MATCH)			\
       FUNCTION(Delay, DELAY)			\
       FUNCTION(Let, LET)			\
       FUNCTION(Post, POST)			\
       FUNCTION(If, IF)						\
       FUNCTION(MakeTuple, MAKETUPLE)				\
       FUNCTION(Substitute, SUBSTITUTE)				\
       FUNCTION(Choose, CHOOSE)					\
       FUNCTION(Constant, CONSTANT)				\
       FUNCTION(Equal, EQUAL)						\
       FUNCTION(Sum, SUM)						\
       FUNCTION(ToString, TOSTRING)					\
       FUNCTION(Concat, CONCAT)		     		\

struct Element : public Base {  
  public:

   #define FUNCTION(func, FUNC) ITEM(FUNC), 
   #define ElementFunctionList {				\
   ALL_FUNCTIONS						\
   };
   CLASS_ENUM_DECLARE(Element, Function);
   #undef FUNCTION

  // ---------- L2 functions ----------  
  void SetTime(OTime new_time);
  void ComputeSetTime() { SetTime(ComputeTime()); }

  // ---------- const functions ----------  
  
  static Function TypeKeywordToFunction(Keyword type) {
    return StringToFunction(Upcase(type.Data())); }

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
  virtual string ToString(int indent = 0) const = 0;
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
  virtual void N1_StoredTimeChanged() { L1_CheckSetTimeViolation();}
  virtual void N1_ComputedTimeChanged() { L1_CheckSetTimeViolation();}

  // a child was connected or disconnected.
  virtual void N1_ChildChanged(int which_child);

  virtual void N1_StoredValueChanged() { CHECK(false);}
  virtual void N1_ComputedValueChanged() { CHECK(false);}

  // ---------- data ----------  

  Link * parent_;
  OTime time_;
};

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  // ---------- L1 functions ----------  
  // ---------- data ----------  


struct StaticElement : public Element {
  // ---------- L2 functions ----------

  // TODO: implement these functions
  void UnlinkFromParent();
  void LinkToParent(StaticElement *parent, int which_child);
  void EraseTree();  
  void SetObject(int which, Object new_value);

  // ---------- const functions ----------  
  Base::Type GetBaseType() const { return Base::STATIC_ELEMENT; }
  virtual bool HasVariableNumChildren() const { return false; }
  bool IsDynamic() const { return false; }
  OTime ComputeTime() const { return CREATION;}
  StaticElement * GetParent() const;
  StaticElement * GetChild(int which) const;
  DynamicElement * GetDynamic(OMap binding) const;
  Object GetObject(int which) const;
  virtual int NumChildren() const = 0;
  int NumDynamicChildren() const { return dynamic_children_->children_.size();}
  virtual int NumObjects() const { return 0;}
  // What new variables are introduced by this node
  virtual set<Variable> GetIntroducedVariables(int which_child) const;
  // What variables exist at this node. 
  // This is the union of GetIntroducedVariables() up the static tree, 
  // not including this node. 
  set<Variable> ComputeVariables() const;
  inline const set<Variable> & GetVariables() const { return variables_;}

  virtual bool ChildrenGoInTuple() const { return HasVariableNumChildren(); }
  virtual bool ElementNeedsSeparateLine() const { return false;}
  virtual bool ChildNeedsSeparateLine(int which_child) const { return false;}

  Record GetRecordForDisplay() const;
  set<Element *> GetAllChildren() const;
  set<StaticElement *> GetAllStaticChildren() const;
  string ToString(int indent = 0) const;
  string TextIdentifier() const { return ToString();}

  
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

  // Create the choices  
  void L1_ClearChoices();
  virtual void L1_CreateChoices(set<Variable> * variables_so_far, Object obj);
  virtual void L1_CreateChoices();

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
  virtual void N1_ObjectChanged(int which) {
    L1_CreateChoices();
  }

  // ---------- data ----------  

  // these links (like all links) are owned by the parent
  MultiLink * dynamic_children_;
  vector<SingleLink *> static_children_;
  vector<Object> objects_;
  vector<Base *> choices_;
  set<Variable> variables_;

};

struct DynamicElement : public Element{
  void EraseTree();  

  // ---------- L2 functions ----------  
  void SetValue(Object new_value);
  void ComputeSetValue(){SetValue(ComputeValue());}
  bool SetBinding(OMap new_binding); // returns false on failure
  // it can figure out the parent from the static node and the bindings.
  // returns false on failure.
  bool ComputeSetBinding() {
    return SetBinding(ComputeBinding());
  }
  bool LinkToParent(); 
  void UnlinkFromParent();

  // ---------- const functions ----------  
  Base::Type GetBaseType() const { return Base::DYNAMIC_ELEMENT;}
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
      + (GetStatic()?GetStatic()->ToString():"NO STATIC")
      + " value=" + value_.ToString();
  }
  int NumChildren() const { return GetStatic()->NumChildren();}
  int NumObjects() const { return GetStatic()->NumObjects();}  
  Object GetObject(int which) const { return GetStatic()->GetObject(which);}
  DynamicElement * GetChild(int which) const {return GetSingleChild(which);}
  DynamicElement * GetSingleChild(int which) const;
  Object GetChildValue(int which) const;
  OMap GetBinding() const { return binding_;}
  DynamicElement * FindParent() const; // finds parent based on bindings.
  virtual Link::Type LinkType(int which_child) const { return Link::SINGLE;}
  OTime ComputeTime() const;
  string ToString(int indent) const;
  virtual Function GetFunction() const { 
    if (!GetStatic()) return Function(-1);
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
  virtual Object ComputeValue() const = 0;
  Object GetValue() const { return value_;}

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

  void L1_CheckSetValueViolation();
  
  // ---------- N1 notifiers ----------  

  // this gets called when the value of a child changes, 
  // or when a child is created(Init) or destroyed(L1_Erase).   
  virtual void N1_ChildValueChanged(int which_child, Object old_val, 
				    Object new_val){ 
    L1_CheckSetValueViolation();
  }

  // called when the binding changes
  virtual void N1_BindingChanged();

  // TODO: forward this
  // Called if the stored or computed value might have changed.
  void N1_ComputedValueChanged() { L1_CheckSetValueViolation();} 
  void N1_StoredValueChanged() { L1_CheckSetValueViolation();}

  // ---------- data ----------  
  MultiLink * static_parent_;
  vector<Link *> children_;
  OMap binding_; // if parent_ is a multilink, always matches it. 
  Object value_;
  
};

// Why aren't these just functions of StaticElement?
#define DECLARE_FUNCTION_ENUMS \
  int StringToChild(string s) const{ return StringToChildName(s);}	\
  string ChildToString(int c) const{					\
    if (HasVariableNumChildren()) return "CHILD(" + itoa(c) + ")";	\
    return ChildNameToString((ChildName)c);}				\
  int StringToObject(string s) const{ return StringToObjectName(s);} \
  string ObjectToString(int o) const{return ObjectNameToString((ObjectName)o);}\
  int NumObjects() const { return NumObjectNames();} \
  int NumChildren() const { return HasVariableNumChildren()?	\
      static_children_.size():NumChildNames();}

struct StaticOn : public StaticElement {
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
  OPattern GetPattern() const { return GetObject(PATTERN);}
  virtual Function GetFunction() const { return ON;}
  set<Variable> GetIntroducedVariables(int which_child) const;
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { return true;}
  // ---------- L1 functions ----------  
  void L1_Init();
  void L1_Erase();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){
    StaticElement::N1_ObjectChanged(which);
    CHECK(NumDynamicChildren() == 0);
  }

  // ---------- data ----------  
};

struct DynamicOn : public DynamicElement {
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
  Object ComputeValue() const { return FALSE;}
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement* parent, OMap dummy);
  void L1_Erase();
  // --------- N1 notifiers ----------
  // ---------- data ----------  
};

struct StaticMatch : public StaticElement {
  #define StaticMatchChildNameList {				\
      ITEM(CHILD),						\
	};
  CLASS_ENUM_DECLARE(StaticMatch, ChildName);
  #define StaticMatchObjectNameList {				\
      ITEM(PATTERN),					\
	};
  CLASS_ENUM_DECLARE(StaticMatch, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  OPattern GetPattern() const { return GetObject(PATTERN);}
  virtual Function GetFunction() const { return MATCH;}
  set<Variable> GetIntroducedVariables(int which_child) const;
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { return false;}
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){
    StaticElement::N1_ObjectChanged(which);
    CHECK(NumDynamicChildren() == 0);
  }

  // ---------- data ----------  
};

struct DynamicMatch : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Link::Type LinkType(int which_child) const{ return Link::MATCH;}
  StaticMatch *GetStaticMatch() const { 
    return dynamic_cast<StaticMatch*>(GetStatic()); }
  MatchMultiLink * GetMatchMultilink() const { 
    return dynamic_cast<MatchMultiLink *>(children_[StaticMatch::CHILD]); }
  OPattern ComputePattern() const {
    return Substitute(GetBinding().Data(), GetStaticMatch()->GetPattern()); }
  OPattern GetCurrentPattern() const { 
    CHECK(GetMatchMultilink());
    return GetMatchMultilink()->GetPattern(); }
  TimedQuery * GetTimedQuery() const {
    CHECK(GetMatchMultilink());
    return GetMatchMultilink()->GetTimedQuery();
  }
  Record GetRecordForDisplay() const;
  Object ComputeValue() const { return Integer::Make(sum_);}
  // ---------- L1 functions ----------
  void L1_Init(StaticElement* parent, OMap binding) {
    sum_ = 0;
    DynamicElement::L1_Init(parent, binding);
    GetTimedQuery()->L1_SetTimeLimit(GetTime());
    GetTimedQuery()->L1_SetPattern(ComputePattern());    
  }
  // --------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    int64 diff = 0;
    if (old_val.GetType() == Object::INTEGER) diff -= Integer(old_val).Data();
    if (new_val.GetType() == Object::INTEGER) diff += Integer(new_val).Data();
    if (diff != 0) {
      CL.ChangeValue(&sum_, sum_+diff);
    }
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
  }
  void N1_BindingChanged() {
    GetTimedQuery()->L1_SetPattern(ComputePattern());    
    DynamicElement::N1_BindingChanged();
  }
  void N1_StoredTimeChanged() {
    DynamicElement::N1_StoredTimeChanged();
    GetTimedQuery()->L1_SetTimeLimit(GetTime());
  }
  // ---------- data ----------
  int64 sum_;
};


struct StaticDelay : public StaticElement { 
  #define StaticDelayChildNameList {				\
      ITEM(DIMENSION),						\
      ITEM(CHILD),						\
	};
  CLASS_ENUM_DECLARE(StaticDelay, ChildName);
  #define StaticDelayObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticDelay, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Function GetFunction() const { return DELAY;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}

  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------    
};
struct DynamicDelay : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  OTime ComputeChildTime(const Link * link, const Element *child) const {
    if (link == children_[StaticDelay::CHILD]) {
      if (time_ == NULL) return OTime();
      DynamicElement *delay_element 
	= GetSingleChild(StaticDelay::DIMENSION);
      if (!delay_element) return OTime();
      Object dimension = delay_element->value_;
      if (dimension.GetType() != Object::OBITSEQ) return OTime();
      return OTime::Make
	(time_.Data() 
	 + OBitSeq(dimension).Data());
    }
    return time_;
  }

  // Dynamic lets can't pass values up, that would allow later actions
  // to influence earlier actions
  Object ComputeValue() const { return FALSE;}

  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    // todo: check whether the times are right on the children
    DynamicElement * child = GetSingleChild(StaticDelay::CHILD);
    if (child) child->N1_ComputedTimeChanged();
  }
  // ---------- data ----------  
};
  
struct StaticLet : public StaticElement {
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
  Variable GetVariable() const { return GetObject(VARIABLE);}
  virtual Function GetFunction() const { return LET;}

  set<Variable> GetIntroducedVariables(int which_child) const {
    if (which_child == CHILD) return Singleton<set<Variable> >(GetVariable());
    return set<Variable>();
  }
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){
    StaticElement::N1_ObjectChanged(which);
    CHECK(NumDynamicChildren() == 0);
  }
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  // ---------- data ----------  
};
struct DynamicLet : public DynamicElement {
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
  Object ComputeValue() const { return GetChildValue(StaticLet::CHILD);}
  // ---------- L1 functions ----------  
  void L1_CheckSetLetViolation(); // (programming with Dr. Seuss)
  void L1_Init(StaticElement * static_parent, OMap binding) {
    DynamicElement::L1_Init(static_parent, binding);
    L1_CheckSetLetViolation();
  }
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_CheckSetLetViolation();
  }
  // ---------- data ----------  
};

struct StaticPost : public StaticElement {
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
  virtual Function GetFunction() const { return POST;}
  bool ElementNeedsSeparateLine() const { return true;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};
struct DynamicPost : public DynamicElement {
  // ---------- L2 functions ----------  
  void AddCorrectPosting();
  void AddPosting(OTuple t, OTime time);
  void RemovePosting();
  void SetPostingTime(OTime new_time);
  void SetCorrectPostingTime() { SetPostingTime(time_);}
  // ---------- const functions ----------  
  bool NeedsPostViolation() const;
  Object ComputeTuple() const{
    DynamicElement * expr = GetSingleChild(StaticPost::TUPLE);
    if (!expr) return Object();
    return expr->GetValue();
  }
  OwnedPosting * GetOwnedPosting() const { return posting_;}
  Record GetRecordForDisplay() const;
  Object ComputeValue() const { return GetChildValue(StaticPost::TUPLE);}
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding) {
    posting_ = NULL;
    DynamicElement::L1_Init(static_parent, binding);
    L1_CheckSetPostViolation();
  }
  void L1_Erase() {
    if (posting_) posting_->L1_Erase();
    DynamicElement::L1_Erase();
  }
  // see if it's perfect, then create or remove violation if necessary.
  void L1_CheckSetPostViolation();
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_CheckSetPostViolation();
  }  
  // ---------- data ----------  
  OwnedPosting * posting_;
};

// if the condition does not evaluate to Boolean::Make(false), 
// then the ON_TRUE should be executed.
struct StaticIf : public StaticElement {
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
  bool ChildrenGoInTuple() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const {
    if (which_child == CONDITION) return false;
    for (int c = ON_TRUE; c<NumChildren(); c++) {
      StaticElement *e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Function GetFunction() const { return IF;}
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------  
};
struct DynamicIf : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  StaticIf *GetStaticIf() const { 
    return dynamic_cast<StaticIf*>(GetStatic());
  }
  bool ChildShouldExist(int which_child) const;
  Object ComputeValue() const { 
    if (ChildShouldExist(StaticIf::ON_TRUE)) 
      return GetChildValue(StaticIf::ON_TRUE);
    if (ChildShouldExist(StaticIf::ON_FALSE)) 
      return GetChildValue(StaticIf::ON_FALSE);
    return NULL;
  }

  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding) {
    DynamicElement::L1_Init(static_parent, binding);
  }
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) {
    /*cerr << "DynamicIf::N1_ChildValueChanged :"
	 << " child="
	 << StaticIf::ChildNameToString(StaticIf::ChildName(which_child))
	 << " old_val=" << old_val
	 << " new_val=" << new_val << endl;*/
    if (old_val == new_val) return;
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    if (which_child == StaticIf::CONDITION) L1_CheckSetChildViolation();
  }
  // ---------- data ----------  
};


struct StaticSubstitute : public StaticElement {
   #define StaticSubstituteChildNameList {	 		\
    ITEM(CHILD),						\
      };
  CLASS_ENUM_DECLARE(StaticSubstitute, ChildName);
  #define StaticSubstituteObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticSubstitute, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  string ToString(int indent) const;

  virtual Function GetFunction() const { return SUBSTITUTE;}
  string ToString() const;
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicSubstitute : public DynamicElement {
  Object ComputeValue() const;
  void N1_BindingChanged() {
    DynamicElement::N1_BindingChanged();
    L1_CheckSetValueViolation();
  }
};

struct StaticEqual : public StaticElement {
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
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicEqual : public DynamicElement {
  Object ComputeValue() const;
};

struct StaticSum : public StaticElement {
   #define StaticSumChildNameList {	 		\
    ITEM(LHS),						\
    ITEM(RHS),						\
      };
  CLASS_ENUM_DECLARE(StaticSum, ChildName);
  #define StaticSumObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticSum, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return SUM;}
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicSum : public DynamicElement {
  Object ComputeValue() const;
};

struct StaticToString : public StaticElement {
   #define StaticToStringChildNameList {	 		\
    ITEM(ARG),						\
      };
  CLASS_ENUM_DECLARE(StaticToString, ChildName);
  #define StaticToStringObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(StaticToString, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return TOSTRING;}
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicToString : public DynamicElement {
  Object ComputeValue() const;
};

struct StaticConcat : public StaticElement {
   #define StaticConcatChildNameList {	 		\
      };
  CLASS_ENUM_DECLARE(StaticConcat, ChildName);
   #define StaticConcatObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(StaticConcat, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return CONCAT;}
  bool HasVariableNumChildren() const { return true; }
};
struct DynamicConcat : public DynamicElement {
  Object ComputeValue() const;
};

struct StaticMakeTuple : public StaticElement {
   #define StaticMakeTupleChildNameList {	 		\
      };
  CLASS_ENUM_DECLARE(StaticMakeTuple, ChildName);
   #define StaticMakeTupleObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(StaticMakeTuple, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return MAKETUPLE;}
  bool HasVariableNumChildren() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    for (int c = 0; c<NumChildren(); c++) {
      StaticElement *e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
};
struct DynamicMakeTuple : public DynamicElement {
  Object ComputeValue() const;
};


struct StaticChoose : public StaticElement { 
   #define StaticChooseChildNameList {	 		\
    ITEM(STRATEGY),					\
      };
  CLASS_ENUM_DECLARE(StaticChoose, ChildName);
  #define StaticChooseObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(StaticChoose, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return CHOOSE;}
};

// For now, we'll make choice_->value_ always match value_, and 
// choice_->parameter_ always match GetChildValue(PARAMETER). 
// This cuts down on violations, but we may need a new violation if the 
// choice is impossible for that parameter.  
struct DynamicChoose : public DynamicElement {
  // ---------- L2 functions ----------  

  // ---------- const functions ----------  
  Object ComputeValue() const;
  
  // ---------- L1 functions ----------  
  void L1_Init(StaticElement * static_parent, OMap binding);
  void L1_Erase();
  bool L1_TryMakeChoice(OTuple strategy, Object value);
  bool L1_TryMakeCorrectChoice();

  // ---------- N1 Notifiers ----------  
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) {
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_TryMakeCorrectChoice();
  }
  void N1_StoredValueChanged() {
    L1_TryMakeCorrectChoice();
    DynamicElement::N1_StoredValueChanged();
  }

  // ---------- data ----------  
  Choice * choice_; // The current choice.
};

struct StaticConstant : public StaticElement {
  #define StaticConstantChildNameList {		\
    };
  CLASS_ENUM_DECLARE(StaticConstant, ChildName);
  #define StaticConstantObjectNameList {		\
    ITEM(OBJECT),						\
    };
  CLASS_ENUM_DECLARE(StaticConstant, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  string ToString(int indent) const;
  virtual Function GetFunction() const { return CONSTANT;}

  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which);

  // ---------- data ----------  

};
struct DynamicConstant : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Object ComputeValue() const;
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};

template <class T> T * MakeStaticElement() {
  CHECK(static_cast<Element *>((T *)NULL) == NULL);
  return New<T>();
}

template <class T> T * MakeDynamicElement(StaticElement *static_parent, 
					  OMap binding) {
  CHECK(static_cast<DynamicElement *>((T *)NULL) == NULL);
  return New<T>(static_parent, binding);
}

DynamicElement * MakeDynamicElement(StaticElement *static_parent, 
				    OMap binding);

#endif
