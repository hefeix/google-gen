// Copyright (C) 2006 Google Inc. and Georges Harik
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
// Author: Noam Shazeer and Georges Harik

#include "changelist.cc"

Changelist cl;
string s;
set<int> my_set;
map<string, string> my_map;
struct Employee {
  Employee(string name) { name_ = name;}
  void Hire() { cout << "Hired " << name_ << endl; }
  void Fire() { cout << "Fired " << name_ << endl; }
  void GiveTenure() { cout << "Gave Tenure to " << name_ << endl; }
  string name_;
};
Employee fred("Fred");
Employee mary("Mary");
void ShowWorldState(){
  cout << "cl_size=" << cl.GetCheckpoint()
       << " s=" << s
       << " set={";
  forall(run, my_set){
    cout << *run << ",";
  }
  cout << "} map={";
  forall(run, my_map) {
    cout << "(" << run->first << "," << run->second << "),";
  }
  cout << "}";
  cout << endl;
}

void TestChangelist(){
  s = "original_string";
  Checkpoint cp0 = cl.GetCheckpoint();
  cl.Make(new SetInsertChange<int>(&my_set, 2)); 
  ShowWorldState();
  cl.Make(new MemberCallChange<Employee>(&mary, &Employee::Hire,
					       &Employee::Fire,
					       &Employee::GiveTenure));
  ShowWorldState();
  cl.MakeChangesPermanent();
  cout << "Made Changes Permanent" << endl;
  cout << "Set checkpoint cp0" << endl;
  cl.Make(new SimpleChange<string>(&s, "new_string")); 
  ShowWorldState();
  cl.Make(new SetInsertChange<int>(&my_set, 4)); 
  ShowWorldState();
  cl.Make(new SetInsertChange<int>(&my_set, 6)); 
  ShowWorldState();
  cl.Make(new SetRemoveChange<int>(&my_set, 4)); 
  ShowWorldState();
  cl.Make(new MapInsertChange<string, string>(&my_map, "key1", "val1")); 
  ShowWorldState();
  cl.Make(new MapInsertChange<string, string>(&my_map, "key2", "val2")); 
  ShowWorldState();
  Checkpoint cp1 = cl.GetCheckpoint();
  cout << "Set checkpoint cp1" << endl;
  cl.Make(new MapRemoveChange<string, string>(&my_map, "key1")); 
  ShowWorldState();
  cl.Make(new MemberCallChange<Employee>(&fred, &Employee::Hire,
					       &Employee::Fire));
  ShowWorldState();
  cl.Make(new MemberCallChange<Employee>(&fred, &Employee::Fire,
					       &Employee::Hire));
  ShowWorldState();
  cout << "Rolling back to cp1" << endl;
  cl.Rollback(cp1);
  ShowWorldState();
  cout << "Rolling back to cp0" << endl;
  cl.Rollback(cp0);
  ShowWorldState();  
}

int main(){
  TestChangelist();
}
