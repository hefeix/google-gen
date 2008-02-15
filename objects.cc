#include <iostream>
#include "objects.h"

template<ObjectType OT, class D>
map<D, SpecificDefinition<OT, D> *> SpecificDefinition<OT, D>::unique_;

//template<ObjectType OT, class D>
//map<string, SpecificDefinition<FLAKE, string> *> SpecificDefinition<FLAKE, string>::unique_;

template<>
string FlakeDefinition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string KeywordDefinition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string VariableDefinition::ToStringSpecific(bool verbose) const { 
  if (data_ < 26) 
    return string() + (char('a' + data_));
  return "v" + itoa(data_);
}

template<>
string TupleDefinition::ToStringSpecific(bool verbose) const {
  string ret = "(";
  for (uint i=0; i<data_.size(); i++) {
    ret += data_[i].ToString(verbose);
    if (i+1<data_.size()) ret += ", ";
  }
  ret += ")";
  return ret;
}

template<>
string BooleanDefinition::ToStringSpecific(bool verbose) const {
  return data_?"true":"false";
}

template<>
string IntegerDefinition::ToStringSpecific(bool verbose) const {
  return itoa(data_);
}

template<>
string RealDefinition::ToStringSpecific(bool verbose) const {
  string ret = dtoa(data_);
  if (ret.find('.')==string::npos && ret.find('e')==string::npos) 
    ret += ".0";
  return ret;
}

template<>
string EscapeDefinition::ToStringSpecific(bool verbose) const {
  return "\'" + data_.ToString();
}

bool IsNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-' || c=='+' || c=='e');
}
bool IsBeginningNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-');
}
bool IsNameChar(char c){
  return (isalnum(c) || c=='_');
}
bool IsOpenEnclosure(char c){
  return (c=='(' || c=='[' || c=='{');
}
char MatchingCloseEnclosure(char c){
  switch(c){
  case '(': return ')';
  case '[': return ']';
  case '{': return '}';
  default: CHECK(false); return ' ';
  }
}
void EatCommentsAndWhitespace(istream & input) {
  input >> ws;
  if (input.peek() == '<') {
    char c;
    input >> c;
    int level = 1;
    while (level > 0) {
      CHECK(input >> c);
      if (c=='<') level++;
      if (c=='>') level--;
    }
    EatCommentsAndWhitespace(input);
  }
  if (input.peek()=='/') {
    input.get();
    CHECK(input.peek()=='/');
    char c;
    while (input.get(c)) if (c=='\n') break;
    EatCommentsAndWhitespace(input);
  }
  input >> ws;
}
void EatCommaAndCommentsAndWhitespace(istream & input) {
  EatCommentsAndWhitespace(input);
  if (input.peek() == ',') input.get();
  EatCommentsAndWhitespace(input);
}
istream & operator >>(istream & input, Object & o){
  EatCommentsAndWhitespace(input);
  char firstchar;
  if (!(input >> firstchar)) return input;
  if (IsOpenEnclosure(firstchar)) { // it's a tuple;
    input >> ws;
    vector<Object> v;
    Object element;
    while (input.peek() != MatchingCloseEnclosure(firstchar)) {
      CHECK(input >> element);
      v.push_back(element);
      EatCommaAndCommentsAndWhitespace(input);
    }
    input.get();
    o = Tuple::Make(v);
    return input;
  } 
  if (firstchar =='\'') { // it's an escape
    Object sub;
    CHECK(input >> sub);
    o = Escape::Make(sub);
    return input;
  } 
  if (IsBeginningNumericChar(firstchar) ) {
    // it's an integer/real    
    string s;
    char c;
    s += firstchar;
    while (IsNumericChar(input.peek())) {
      input >> c;
      s+=c;
    }
    if (s.find('.')!=string::npos 
	|| s.find('e')!=string::npos) { // it's a double
      double d = atof(s.c_str());
      o = Real::Make(d);
      return input;
    } 
    // it's an integer
    int i = atoi(s.c_str());
    o = Integer::Make(i);
    return input;
  } 

  CHECK(IsNameChar(firstchar));
  string s;
  s += firstchar;
  char c;
  while (IsNameChar(input.peek())){
    input >> c;
    s.push_back(c);
  }
  if (isupper(s[0])) { // it's a flake
    o = Flake::Make(s);
    return input;
  } 
  CHECK(islower(s[0]));
  if (s.size() == 1) {
    o = Variable::Make(s[0]-'a');
    return input;
  } 
  if (isdigit(s[1])) {
    CHECK(s[0]=='v')
    o = Variable::Make(atoi(&(s[1])));
    return input;
  } else {
    if (s=="true") {
      o = Boolean::Make(true);
      return input;
    }
    if (s=="false") {
      o = Boolean::Make(false);
      return input;
    }
    o = Keyword::Make(s);
    return input;
  }
  CHECK(false);
}

int main() {
  Object o;
  vector<Object> v;
  while (cin >> o) {
    //    cout << o.ToString() << endl;
    cout << o.ToString(true) << endl << endl;
    v.push_back(o);
    if (o==Keyword::Make("clear")) v = vector<Object>();
    if (o==Keyword::Make("done")) break;
  }
  /*Variable seven = Variable::Make(7);
  Flake n = Flake::Make("Noam");
  Flake n2 = Flake::Make("Noam");
  Flake g = Flake::Make("Georges");
  Flake g2 = g;
  cout << "n=n2 " << ((n==n2)?"T":"F") << endl;
  cout << "n=g " << ((n==g)?"T":"F") << endl;
  cout << "g=g2 " << ((g==g2)?"T":"F") << endl;
  vector<Object> v;
  v.push_back(n);
  v.push_back(g);
  v.push_back(seven);
  Tuple t = Tuple::Make(v);
  cout << n.ToString() << " " 
       << g.ToString() << " " 
       << seven.ToString() << " " 
       << t.ToString() << endl;
  */

}
