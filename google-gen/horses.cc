// Copyright (C) 2006 Google Inc.
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
// Author: Noam Shazeer


#include <iostream.h>
#include <stdlib.h>
#include <vector>
#include <string>
using namespace std;

typedef unsigned long uint32;

uint32 Rand() { // HUH???
  return (rand() / 73) + (rand() / 464 * 23431);
}

class Horses{
public:
  int size_;
  vector<string> words_;
  vector<int> positions_;
  string visible_;
  int timestep_;
  bool log_propositions_;
  
  Horses(int size, vector<string> words, bool log_propositions){
    words_ = words;
    positions_.clear(); 
    for (int i=0; i<words_.size(); i++) positions_.push_back(0);
    log_propositions_ = log_propositions;
    size_ = size;
    timestep_=0;
    ComputeVisible();
    if (log_propositions_) {
      for (int i=0; i<size_; i++)
	printf("[ Successor POS_%d *POS_%d ]\n", i, (i+1)%size_);
    }
  }

  void Visible(int pos, int * word_num, char * letter){
    for (int i=0; i<words_.size(); i++) {
      int let = (pos - positions_[i]+size_)%size_;
      if (let < words_[i].size()) {
        if (word_num) *word_num = i;
	if (letter) *letter = words_[i][let];
	return;
      }
    }
    if (word_num) *word_num = -1;
    if (letter) *letter = '.';
  }

  void Step(int pos, bool dir) {
    int word_num;
    Visible(pos, &word_num, 0);
    if (word_num >= 0) {
      positions_[word_num] = (positions_[word_num] + (dir?1:-1) + size_) % size_;
    }
    if (log_propositions_) {
      // printf("[ TIME_%d POS_%d CHAR_]");
      printf("[ ACT TIME_%d *POS_%d *%s ]\n", timestep_, 
	     pos, (dir?"RIGHT":"LEFT"));
      printf("[ SUC TIME_%d *TIME_%d ]\n", timestep_, timestep_+1);
    }
    timestep_++;
    ComputeVisible();
  }

  void ComputeVisible(){
    visible_ = "";
    for (int pos=0; pos<size_; pos++) {
      char c;
      Visible(pos, 0, &c);
      visible_ += c;
      if (log_propositions_) {
	printf("[ OBS TIME_%d POS_%d *CHAR_%c ]\n", timestep_, pos, c);
	//printf("OBS_%d_%d AT_TIME TIME_%d\n", timestep_, pos, timestep_);
	//printf("OBS_%d_%d AT_POS POS_%d\n", timestep_, pos, pos);
	//printf("OBS_%d_%d OBSERVES CHAR_%c\n", timestep_, pos, c);
      }
    }
  }
  void Run(int n) {
    for (int i=0; i<n; i++) {
      Step(Rand()%size_, Rand()%2);
      if (!log_propositions_) {
	cout << visible_ << endl;
      }
    }
  }
};

int main(){
  vector<string> v;
  v.push_back("cat");
  v.push_back("r");
  v.push_back("dog");
  Horses h(5, v, true);

  //v.push_back("horses");
  //v.push_back("r");
  //v.push_back("elephants");
  //v.push_back("cows");
  //Horses h(12, v, true);
  h.Run(1000);  
}
