#include <iostream>
#include "objects.h"

template<ObjectType OT, class D>
map<D, SpecificDefinition<OT, D> *> SpecificDefinition<OT, D>::unique_;

//template<ObjectType OT, class D>
//map<string, SpecificDefinition<FLAKE, string> *> SpecificDefinition<FLAKE, string>::unique_;

template<>
string FlakeDefinition::ToString() const { return data_; }

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
  return dtoa(data_);
}

template<>
string EscapeDefinition::ToString() const {
  return "^" + data_.ToString();
}

int main() {
  Variable seven = Variable::Make(7);
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


}
