#include <iostream>
#include "objects.h"

template<ObjectType OT, class D>
map<D, SpecificDefinition<OT, D> *> SpecificDefinition<OT, D>::unique_;

//template<ObjectType OT, class D>
//map<string, SpecificDefinition<FLAKE, string> *> SpecificDefinition<FLAKE, string>::unique_;

template<>
string FlakeDefinition::ToString() const { return data_; }

template<>
string KeywordDefinition::ToString() const { return data_; }

template<>
string VariableDefinition::ToString() const { 
  if (data_ < 26) 
    return string() + (char('a' + data_));
  return "v" + itoa(data_);
}

template<>
string TupleDefinition::ToString() const {
  string ret = "(";
  for (uint i=0; i<data_.size(); i++) {
    ret += data_[i].ToString();
    if (i+1<data_.size()) ret += ", ";
  }
  ret += ")";
  return ret;
}

template<>
string BooleanDefinition::ToString() const {
  return data_?"true":"false";
}

template<>
string IntegerDefinition::ToString() const {
  return itoa(data_);
}

template<>
string RealDefinition::ToString() const {
  string ret = dtoa(data_);
  if (ret.find('.')==string::npos && ret.find('e')==string::npos) 
    ret += ".0";
  return ret;
}

template<>
string EscapeDefinition::ToString() const {
  return "\'" + data_.ToString();
}

void EatCommaAndWhitespace(istream & input) {
  input >> ws;
  if (input.peek() == ',') input.get();
  input >> ws;
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
istream & operator >>(istream & input, Object & o){
  char firstchar;
  if (!(input >> firstchar)) return input;
  if (firstchar == '(') { // it's a tuple;
    input >> ws;
    vector<Object> v;
    Object element;
    while (input.peek() != ')') { // it's a tuple
      CHECK(input >> element);
      v.push_back(element);
      EatCommaAndWhitespace(input);
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
    cout << o.ToString() << endl;
    cout << o.ToStringVerbose() << endl << endl;
    v.push_back(o);
    if (o==Keyword::Make("clear")) v = vector<Object>();
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
