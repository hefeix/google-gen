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
#include "execution.h"

#define ALL_FUNCTIONS \
  FUNCTION(On, ON)						\
       FUNCTION(Match, MATCH)					\
       FUNCTION(MatchLast, MATCH_LAST)				\
       FUNCTION(MatchRandom, MATCH_RANDOM)			\
       FUNCTION(MatchRandom, MATCH_COUNT)			\
       FUNCTION(Post, POST)					\
       FUNCTION(MakeTuple, MAKETUPLE)				\
       FUNCTION(Substitute, SUBSTITUTE)				\
       FUNCTION(Constant, CONSTANT)				\
       FUNCTION(Equal, EQUAL)				     	\
       FUNCTION(Sum, SUM)					\
       FUNCTION(If, IF)					       	\
       FUNCTION(Cout, COUT)					\
       FUNCTION(Nth, NTH)					\
       FUNCTION(Let, LET)					\
       FUNCTION(Delay, DELAY)					\
       FUNCTION(AddCode, ADD_CODE)				\
       FUNCTION(NewFlake, NEW_FLAKE)				\
       FUNCTION(RandomBool, RANDOM_BOOL)	       		\
       FUNCTION(RandomUint, RANDOM_UINT)		        \

       /*
    

       FUNCTION(Choose, CHOOSE)		\
       */


struct Element : public Base {  
 public:
  
  #define FUNCTION(func, FUNC) ITEM(FUNC), 
  #define ElementFunctionList {					\
    ALL_FUNCTIONS						\
      };
  CLASS_ENUM_DECLARE(Element, Function);
  #undef FUNCTION

  // Creation / destruction
  Element() : parent_(NULL) {};
  void Init() { Base::Init(); }
  void Erase() { Base::Erase(); }

  // Run things
  virtual Object Execute(Thread thread);
  virtual Object ComputeReturnValue(Thread thread, Tuple results) {
    CHECK(false); return NULL;
  }

  // Verification code
  bool VerifyNode() {
    return ( (RequiredNumChildren() == NumChildren()) ||
	    HasVariableNumChildren());
  }

  bool VerifyTree() {
    if (!VerifyNode()) return false;
    for (uint c=0; c<children_.size(); c++) {
      Element * child = children_[c];
      if (!child->VerifyTree()) return false;
    }
    return true;
  }

  // Linking things up together
  void AddChild(Element * child) {
    CHECK(child->parent_ == NULL);
    child->parent_ = this;
    children_.push_back(child);
  }
  
  // This is for parsing
  static Function TypeKeywordToFunction(Keyword type) {
    return StringToFunction(Upcase(type.Data())); 
  }
 
  // Printing
  Record GetRecordForDisplay() const;
  virtual string ProgramTree(int indent = 0) const;
  string TextIdentifier() const { return ProgramTree(); }
  
  // Simple accessor functions for the static tree
  Element * GetParent() const { return parent_; }
  Element * GetChild (int which) const { 
    CHECK(which >= 0 && which < NumChildren());
    return children_[which]; 
  }
  virtual int RequiredNumChildren() const = 0;
  int NumChildren() const { return children_.size(); }
  
  // Accessing the function of the element
  Keyword FunctionKeyword() const { 
    return Keyword::Make(Downcase(FunctionToString(GetFunction())));
  }
  virtual Function GetFunction() const = 0;
  
  // Accessing children
  int WhichChildAmI() const { 
    if (parent_) return parent_->WhichChildIsThis(this); 
    return -1; 
  }
  int WhichChildIsThis(const Element * child) const {
    for (uint c=0; c<children_.size(); c++)
      if (children_[c] == child) return c;
    return -1;
  }
  
  virtual bool HasVariableNumChildren() const { return false; }
  
  // Accessing names of children pnemonically
  // These are virtual but don't need to be defined by subclasses other
  // than by the macros
  virtual int StringToChild(string s) const = 0;
  virtual string ChildToString(int c) const = 0;

  // Sometimes overridden
  virtual bool ChildrenGoInTuple() const { return HasVariableNumChildren(); }
  virtual bool ElementNeedsSeparateLine() const { return false; }
  virtual bool ChildNeedsSeparateLine(int which_child) const { return false; }

  // Doesn't need to be overridden
  Base::Type GetBaseType() const { return Base::ELEMENT; }

  static void StaticInit();

  // ---------- data ----------  
  Element * parent_;
  vector<Element *> children_;
};

// We want to call static functions in the right class given a particular
// C object. Therefore, we forward to virtual functions in each class,
// which then call the appropriate static functions. These virtual functions
// are declared and defined by this macro.
#define DECLARE_FUNCTION_ENUMS \
  int StringToChild(string s) const{ return StringToChildName(s); }	\
  string ChildToString(int c) const{					\
    if (HasVariableNumChildren()) return "CHILD(" + itoa(c) + ")";	\
    return ChildNameToString((ChildName)c);}				\
  int RequiredNumChildren() const { return HasVariableNumChildren()?	\
      -1:NumChildNames();}

template <class T> T * MakeElement() {
  CHECK(static_cast<Element *>((T *)NULL) == NULL);
  return New<T>();
}

struct MatchBaseElement : public Element {
#define MatchBaseElementChildNameList { ITEM(TUPLE), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(MatchBaseElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  
  bool ElementNeedsSeparateLine() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const { 
    if (which_child == CHILD) return true;
    return false;
  }
  Object Execute(Thread thread);
  virtual Object SubclassExecute(Thread thread, Object variable_tuple) = 0;
};

struct OnElement : public MatchBaseElement {
  virtual Function GetFunction() const { return ON; }
  Object SubclassExecute(Thread thread, Object variable_tuple);
};

struct MatchElement : public MatchBaseElement {
  Function GetFunction() const { return MATCH; }
  Object SubclassExecute(Thread thread, Object variable_tuple);
};

struct MatchRandomElement : public MatchBaseElement {
  Function GetFunction() const { return MATCH_RANDOM; }
  Object SubclassExecute(Thread thread, Object variable_tuple);
};

// may want to combine with MatchRandom into MatchOne 
// to save 8 lines of code.
struct MatchLastElement : public MatchBaseElement {
  Function GetFunction() const { return MATCH_LAST; }
  Object SubclassExecute(Thread thread, Object variable_tuple);
};

struct MatchCountElement : public Element {
#define MatchCountElementChildNameList { ITEM(TUPLE) };
  CLASS_ENUM_DECLARE(MatchCountElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return MATCH_COUNT; }
  bool ElementNeedsSeparateLine() const { return true; }
  Object Execute(Thread thread);
};

struct PostElement : public Element {
#define PostElementChildNameList { ITEM(TUPLE) };
  CLASS_ENUM_DECLARE(PostElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return POST;}
  bool ElementNeedsSeparateLine() const { return true; }
  Object Execute(Thread thread);
};

struct ConstantElement : public Element {
#define ConstantElementChildNameList {};
  CLASS_ENUM_DECLARE(ConstantElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return CONSTANT; }
  string ProgramTree(int indent) const;
  void SetObject(Object o) { object_ = o; }
  Object Execute(Thread t) { return object_; }
  Object object_;
};

struct MakeTupleElement : public Element {
#define MakeTupleElementChildNameList {};
  CLASS_ENUM_DECLARE(MakeTupleElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return MAKETUPLE; }
  bool HasVariableNumChildren() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    for (int c = 0; c<NumChildren(); c++) {
      const Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    return OTuple::Make(results);
  }
};

struct SubstituteElement : public Element {
#define SubstituteElementChildNameList { ITEM(CHILD) };
  CLASS_ENUM_DECLARE(SubstituteElement, ChildName);
  DECLARE_FUNCTION_ENUMS;

  string ProgramTree(int indent) const;
  virtual Function GetFunction() const { return SUBSTITUTE;}
  string ToString() const;
  bool ChildrenGoInTuple() const { return true;}
  Object ComputeReturnValue(Thread thread, Tuple results) {
    return DeepSubstitute(thread.binding_, results[CHILD]);
  }
};

struct EqualElement : public Element {
#define EqualElementChildNameList { ITEM(LHS), ITEM(RHS), };
  CLASS_ENUM_DECLARE(EqualElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return EQUAL; }
  bool ChildrenGoInTuple() const { return true; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[0] == results[1]) return TRUE;
    return FALSE;
  }
};

struct SumElement : public Element {
#define SumElementChildNameList { ITEM(TUPLE), };
  CLASS_ENUM_DECLARE(SumElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return SUM; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[0].GetType() != Object::OTUPLE) return NULL;
    const Tuple & t = OTuple(results[0]).Data();
    int sum = 0;
    forall(run, t) {
      if (run->GetType() == Object::INTEGER) sum += Integer(*run).Data();
    }
    return Integer::Make(sum);
  }
};

struct IfElement : public Element {
#define IfElementChildNameList {					\
    ITEM(CONDITION),							\
      ITEM(ON_TRUE),							\
      ITEM(ON_FALSE),							\
      };
  CLASS_ENUM_DECLARE(IfElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return IF; }
  bool ChildrenGoInTuple() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    if (which_child == CONDITION) return false;
    for (int c = ON_TRUE; c<NumChildren(); c++) {
      Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Object Execute(Thread thread);
};

struct CoutElement : public Element {
#define CoutElementChildNameList { ITEM(CHILD), };
  CLASS_ENUM_DECLARE(CoutElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return COUT; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    thread.execution_->output_ += results[0].ToString() + '\n';
    return results[0];
  }
};

struct NthElement : public Element {
#define NthElementChildNameList { ITEM(N), ITEM(TUPLE)};
  CLASS_ENUM_DECLARE(NthElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return NTH; }
  bool ChildrenGoInTuple() const { return true; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[N].GetType() != Object::INTEGER) return NULL;
    if (results[TUPLE].GetType() != Object::OTUPLE) return NULL;
    int n = Integer(results[N]).Data();
    const Tuple & t = OTuple(results[TUPLE]).Data();
    if (n<0 || n>=(int)t.size()) return NULL;
    return t[n];
  }
};

struct LetElement : public Element {
#define LetElementChildNameList { ITEM(VARIABLE), ITEM(VALUE), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(LetElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return LET;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  Object Execute(Thread thread);
};

struct DelayElement : public Element {
#define DelayElementChildNameList { ITEM(DIMENSION), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(DelayElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return DELAY;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  Object Execute(Thread thread);
};

struct RandomBoolElement : public Element {
#define RandomBoolElementChildNameList { ITEM(PRIOR), };
  CLASS_ENUM_DECLARE(RandomBoolElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return RANDOM_BOOL; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[PRIOR].GetType() != Object::Real) return NULL;
    double prior = Real(results[PRIOR]).Data();
    bool ret = (RandomFraction() < prior);
    return Boolean::Make(ret);
  }
};

struct RandomUintElement : public Element {
#define RandomUintElementChildNameList { };
  CLASS_ENUM_DECLARE(RandomUintElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return RANDOM_UINT; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    int ret = RandomUintQuadratic();
    return Integer::Make(ret);
  }
};



#endif
