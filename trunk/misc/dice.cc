// Copyright (C) 2008 Google Inc.
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
// Author; Noam Shazeer


// Code to test a method for searching for distributions to approximate the
// posteriors on a bayes net with evidence.  

/*
  Code for testing the method of estimating and optimizing the Shazeer score
  to find an approximation to the posteriors on a Bayes net with evidence.

  We test the method on a toy problem:
  We roll a number of dice, and observe the sum.  


*/

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <cmath>

using namespace std;

#define forall(A, B) for ( typeof((B).begin()) A = (B).begin(); A!=(B).end(); ++A )

double RandomFraction(){ return (rand() + 0.5) / RAND_MAX; }

const int kSidesOnADie = 6;
const int kNumDice = 10;
const int kRequiredSum = 4;
const bool kComputeExact = false;

double DiePriors(int which_die, int which_side) {
  return 1.0/kSidesOnADie;
  static vector<vector<double> > weights;
  if (weights.size() == 0) {
    for (int d=0; d<kNumDice; d++) {
      vector<double> v;
      double sum = 0;
      for (int i=0; i<kSidesOnADie; i++) {
	v.push_back(exp(RandomFraction() * 3));
	sum += v[i];
      }
      for (int i=0; i<kSidesOnADie; i++) v[i]/=sum;
      weights.push_back(v);    
    }
  }
  return weights[which_die][which_side];
}




// (# of dice rolled so far, sum of those dice)
typedef pair<int, int> State;

// For each state, distribution over the next die
typedef  map<State, map<int, double> > GuideAlgorithm;
typedef vector<int> Assignment;


void Normalize(map<int, double> *m){
  double sum = 0;
  forall(run, *m) sum += run->second;
  forall(run, *m) run->second /= sum;
}






ostream & operator << (ostream &output, const Assignment &v) {
  output << "( ";
  for (int i=0; i<v.size(); i++) output << v[i] << " ";
  output << ")";
  return output;
}

ostream & operator << (ostream &output, const GuideAlgorithm & s) {
  output << "{" << endl;
  forall(run_state, s) {
    output << "  (" << run_state->first.first 
	   << ", " << run_state->first.second << ") ";
    forall(run_val, run_state->second) {
      output << run_val->first << ":" << run_val->second << " ";
    }
    output << endl;
  }
  output << "}" << endl;
  return output;
}



int PickFromDistribution(const map<int, double> &dist, 
			 double *random_fraction = 0) {
  double r = (random_fraction?(*random_fraction):RandomFraction());
  forall(run, dist) {
    r -= run->second;
    if (r<0) return run->first;
  }
  return -1;
}

Assignment Sample(GuideAlgorithm &a, 
		  vector<double> * random_fractions = 0) {
  Assignment ret;
  int sum = 0;
  for (int i=0; i<kNumDice; i++) {
    int n = PickFromDistribution(a[make_pair(i, sum)], 
				 random_fractions?(&(*random_fractions)[i]):0);
    ret.push_back(n);
    sum+=n;
  }
  return ret;
}
pair<Assignment, Assignment> ParallelSample(GuideAlgorithm &a, 
					    GuideAlgorithm &b) {
  vector<double> r;
  for (int i=0; i<kNumDice; i++) {
    r.push_back(RandomFraction());
  }
  return make_pair(Sample(a, &r), Sample(b, &r));
}

double TrueLnPrior(const Assignment &a) {
  int sum = 0;
  double ret = 0;
  for (int d=0; d<kNumDice; d++) {
    ret += log(DiePriors(d, a[d]));
    sum += a[d];
  }
  if (sum != kRequiredSum) ret += log(0);
  return ret;
}


double LnLikelihood(GuideAlgorithm &alg, const Assignment & asgn) {
  int sum = 0;
  double ret = 0;
  for (int d=0; d<kNumDice; d++) {
    ret += log(alg[make_pair(d, sum)][asgn[d]]);
     sum += asgn[d];
  }
  return ret;
}

// returns all assignemnts such that P(x,e) > 0
// the second value is P(x,e)
map<Assignment, double> AllLegalAssignments(double *total_likelihood = 0) {
  if (total_likelihood) *total_likelihood = 0;
  map<Assignment, double> ret;
  for (int combination=0; 
       combination < pow((double)kSidesOnADie, kNumDice); 
       combination++) {
    Assignment dice;
    int tmp = combination;
    int sum = 0;
    double likelihood = 1.0;
    for (int d=0; d<kNumDice; d++) {
      dice.push_back(tmp%kSidesOnADie);
      tmp/=kSidesOnADie;
      sum += dice[d];
      likelihood *= DiePriors(d, dice[d]);
    }
    if (sum!=kRequiredSum) likelihood = 0;
    if (likelihood > 0) ret[dice] = likelihood;
    if (total_likelihood) *total_likelihood += likelihood;
  }
  return ret;
}

double KLDivergence(GuideAlgorithm & a1, GuideAlgorithm & a2) {
  double ret = 0;
  map<Assignment, double> assignemnts = AllLegalAssignments();
  forall(run, assignemnts) {
    Assignment assignment = run->first;
    double ll1 = LnLikelihood(a1, assignment);
    double ll2 = LnLikelihood(a2, assignment);
    if (!finite(ll1)) continue;    
    ret += exp(ll1) * (ll1-ll2);
  }
  return ret;
}

// returns a guide algorithm that samples from the true posteriors
GuideAlgorithm TruePosteriors() {
  GuideAlgorithm ret;
  map<Assignment, double> assignemnts = AllLegalAssignments();
  forall(run, assignemnts) {
    Assignment dice = run->first;
    double likelihood = run->second;
    int sum = 0;
    for (int d=0; d<kNumDice; d++) {
      ret[make_pair(d, sum)][dice[d]] += likelihood;
      sum += dice[d];
    }
  }
  forall(run_state, ret) Normalize(&(run_state->second));
  return ret;
}

bool IsPossibleNextNumber(int dice_so_far, int sum_so_far, int n) {
  int min_total = sum_so_far + n;
  int max_total = sum_so_far + n 
    + (kNumDice - dice_so_far - 1) * (kSidesOnADie-1);
  return (kRequiredSum >= min_total && kRequiredSum <= max_total);
}

GuideAlgorithm RandomGuideAlgorithm() {
  GuideAlgorithm ret;
  for (int dice_so_far=0; dice_so_far < kNumDice; dice_so_far++) {
    for (int sum = 0; sum <= dice_so_far * (kSidesOnADie-1); sum++) {
      for (int n=0; n<kSidesOnADie; n++) {
	if (IsPossibleNextNumber(dice_so_far, sum, n))
	  ret[make_pair(dice_so_far, sum)][n] = RandomFraction();
      }
    }
  }
  forall(run_state, ret) Normalize(&(run_state->second));  
  return ret;
}

double TrueS(GuideAlgorithm &s) {
  double ret = 0;
  map<Assignment, double> assignemnts = AllLegalAssignments();
  forall(run, assignemnts) {
    double ll = LnLikelihood(s, run->first);
    if (!finite(ll)) continue;
    ret += exp(ll) * (TrueLnPrior(run->first) - ll);
  }
  return ret;
}
double EstimateS(GuideAlgorithm &g, int trials) {
  double ret = 0;
  for (int i=0; i<trials; i++) {
    Assignment asgn = Sample(g);
    ret += TrueLnPrior(asgn) 
      - LnLikelihood(g, asgn);
  }
  return ret / trials;
}
double EstimateSDelta(GuideAlgorithm &a, GuideAlgorithm &b, int trials) {
  double ret = 0;
  for (int i=0; i<trials; i++) {
    pair<Assignment, Assignment> asgn = ParallelSample(a, b);
    ret += (TrueLnPrior(asgn.first) - LnLikelihood(a, asgn.first)) -
      (TrueLnPrior(asgn.second) - LnLikelihood(a, asgn.second));
  }
  return ret / trials;
}

GuideAlgorithm Perturb(const GuideAlgorithm &g) {
  GuideAlgorithm t = g;
  while(1) {
    int which = rand() % t.size();
    GuideAlgorithm::iterator run = t.begin();
    for (int i=0; i<which; i++) run++;
    map<int, double> & dist = run->second;
    if (dist.size() == 1) continue;
    int which_val = rand() % dist.size();
    map<int, double>::iterator run_d = dist.begin();
    for (int i=0; i<which_val; i++) run_d++;
    double &val = run_d->second;
    val += (RandomFraction() * 2 - 1) * 0.3;
    if (val < 0) val = 0;
    Normalize(&dist);
    return t;
  }
}

int main() {
  srand(time(NULL));

  GuideAlgorithm t;
  if (kComputeExact) {
    double pe; // P(e)
    map<Assignment, double> ala = AllLegalAssignments(&pe);
    cout << "All legal assignments" << endl;
    forall(run, ala) {
      cout << run->first << " " << run->second << endl;
    }
    cout << "P(e) = " << pe << endl;
    cout << endl;

    t = TruePosteriors();
    cout << "t=" << t << endl;
  }


  GuideAlgorithm s = RandomGuideAlgorithm();
  //cout << "s=" << s << endl;

  for (int step = 0; step <= (1<<20) ; step++) {
    if ((step & (step-1)) == 0) {      
      cout << "step " << step;
      if (kComputeExact) {
	cout << "   DKL(s,t) = " << KLDivergence(s, t)
	     << "   TrueS(s) = " << TrueS(s)
	     << "  Est(s, 1000) error = " 
	     << (EstimateS(s, 1000) - TrueS(s))
	     << endl;
      } else {
	cout << "  EstimateS(s, 1000) = " << EstimateS(s, 1000)
	     << endl;
      }
      cout << "s= " << s << endl;
    }
    GuideAlgorithm p = Perturb(s);
    int trials = 1000;
    bool parallel = true;
    double s_delta = 0;
    if (parallel) {
      s_delta = EstimateSDelta(p,s,trials);
    } else {
      s_delta =  (EstimateS(p,trials) - EstimateS(s,trials));
    }
  if (s_delta > 0) s = p;
  }
}
