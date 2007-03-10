#include <iostream.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <vector>
#include <string>
#include <map>
#include <set>

using namespace std;

typedef unsigned long uint32;
int next_object = 0;

set<int> people;
set<int> states;
set<int> colleges;
set<pair<int, int> > friendship;   // <person, person>
set<pair<int, int> > applications; // <person, college>
set<pair<int, int> > acceptances;  // <person, college>
map<int, int> attendance;          // <person, college>
set<pair<int, int> > located;      // <location, thing>
map<int, int>        located_map;  // thing->location

int sum_college_sizes = 0;

// HERE
map<int, int> c_difficulty;        // <college, difficulty>
map<int, int> c_size;              // <college, size>

uint32 Rand() {
  return (rand() / 73) + (rand() / 464 * 23431);
}

double RandDouble() {
  return double(Rand()) / pow(2,32);
}

#define RandomElement(A, B) typeof(B.begin()) A = B.begin(); int howfar = Rand() % B.size(); for (int count=0; count<howfar; count++) ++A;
#define forall(A, B) for ( typeof((B).begin()) A = (B).begin(); A!=(B).end(); ++A )

void MakeStates(int num) {
  for (int c=0; c<num; c++) 
    states.insert(next_object++);
}

void MakePeople(int num) {
  for (int c=0; c<num; c++) {
    people.insert(next_object);
    RandomElement(state, states);
    located.insert(make_pair(*state, next_object));
    located_map[next_object] = *state;
    next_object++;
  }
}

void MakeColleges(int num) {
  for (int c=0; c<num; c++) {
    colleges.insert(next_object);

    // difficulty is 1-5
    int difficulty = 1 + Rand() % 5;
    c_difficulty[next_object] = difficulty;

    // Size is 1-5
    int size = 1 + Rand() % 5;
    c_size[next_object] = size;
    sum_college_sizes += size;

    // College is in some state
    RandomElement(state, states);
    located.insert(make_pair(*state, next_object));
    located_map[next_object] = *state;
    next_object++;
  }
}

int RandomCollegeBySize() {
  int ret = -1;
  int pick = Rand() % sum_college_sizes;
  int sum = -1;
  forall (run, colleges) {
    sum += c_size[*run];
    if (sum >= pick) {
      ret = *run;
      break;
    }
  }
  return ret;
}

int RandomFriend(int person) {
  if (person == -1) return -1;
  int ret = -1;
  set<pair<int, int> >::iterator look = 
    friendship.lower_bound(make_pair(person, -1));
  if (look == friendship.end()) return ret;
  double num_looked = 0.0;
  while ( (look != friendship.end()) && (look->first == person) ) {
    if (RandDouble() < 1/(num_looked+1))
      ret = look->second;
    num_looked++;
    look++;
  }
  return ret;
}

// Pick a random person from that person's state
int RandomPersonFromState(int person) {
  if (person == -1) return -1;
  int ret = -1;
  int state = located_map[person];
  set<pair<int, int> >::iterator object 
    = located.lower_bound(make_pair(state, -1));
  double people_looked = 0.0;
  while ( (object != located.end()) && (object->first == state)) {
    if (people.find(object->second) != people.end()) {
      if (RandDouble() < 1/(people_looked+1)) {
	ret = object->second;
      }
      people_looked++;
    }
    object++;
  }
  return ret;
}

void AddFriend(int person, int person2) {
  if (person == -1) return;
  if (person2 == -1) return;
  if (person == person2) return;
  friendship.insert(make_pair(person, person2));
  friendship.insert(make_pair(person2, person));
}

void MakeFriends(int expected) {
  // make expected * num_people connections
  // A person chooses another person as a friend
  
  int num_friends = 0;
  for (int c=0; c<expected * people.size() * 2; c++) {
    if (!(c%100)) cout << "Step " << c << endl;
    RandomElement(person, people);

    // Lose some friends
    set<pair<int, int> >::iterator friends 
      = friendship.lower_bound(make_pair(*person, -1));
    vector<int> delete_friends;
    while ((friends != friendship.end()) &&
	   (friends->first == *person)) {
      if (RandDouble() < double(1/double(expected)))
	delete_friends.push_back(friends->second);
      friends++;
    }
    for (int c2=0; c2<(int)delete_friends.size(); c2++) {
      friendship.erase(make_pair(*person, delete_friends[c2]));
      friendship.erase(make_pair(delete_friends[c2], *person));
    }

    // gain a friend
    double friend_choice = RandDouble();

    // Pick anyone in the world
    if (friend_choice < 0.1) {
      RandomElement(person2, people);
      AddFriend(*person, *person2);
    } else if (friend_choice < 0.4) {
      // Pick someone in your state
      int person2 = RandomPersonFromState(*person);
      AddFriend(*person, person2);
    } else {
      // Pick a friend of a friend
      int person2 = RandomFriend(*person);
      int person3 = RandomFriend(person2);
      if ( (person2 > 0) && (person3 > 0) )
	cout << "friend of friend\n";
      AddFriend(person2, person3);
    }
  }
}

void PickColleges(int expected) {

  // Every person chooses around num colleges to apply to ...
  for (int c=0; c < expected * people.size() * 5; c++) {
    if (!(c%100)) cout << "Step " << c << endl;
    RandomElement(person, people);

    // Lose some applications
    set<pair<int, int> >::iterator app
      = applications.lower_bound(make_pair(*person, -1));
    vector<int> delete_colleges;
    while ((app != applications.end()) &&
	   (app->first == *person)) {
      if (RandDouble() < double(1/double(expected)))
	delete_colleges.push_back(app->second);
      app++;
    }
    for (int c2=0; c2<(int)delete_colleges.size(); c2++) {
      applications.erase(make_pair(*person, delete_colleges[c2]));
    }

    // Gain an application
    double friend_choice = RandDouble();

    // Pick any college in the world
    int college = -1;
    if (friend_choice < 0.1) {
      // Pick a random college
      college = RandomCollegeBySize();
    } else if (friend_choice < 0.4) {
      // Pick a college from state
      //college = RandomStateCollegeBySize();
    } else {
      // Pick a friend's college
      //college = RandomCollegeByFriend();
    }    
    if (college != -1)
      applications.insert(make_pair(*person, college));
  }

  // Filter out acceptances and pick attendances
  int on_person = -1; 
  double seen = 0.0;
  vector<int> choices;
  forall (run, applications) {
    if (run->first != on_person) {
      seen = 0.0;
      on_person = run->first; 
    }

    // did they get accepted
    if (RandDouble() < (1.0/double(c_difficulty[run->second]))) {
      acceptances.insert(make_pair(run->first, run->second));
      if (RandDouble() < 1.0/(seen+1)) {
	attendance[run->first] = run->second;
	//cout << "ACCEPTED AND ATTENDED " 
	//     << "person " << run->first 
	//     << " college" << run->second << endl;
      } else {
	//cout << "DECLINED" << endl;
      }
      seen += 1.0;
    } else {
      // cout << "REJECTED" << endl;
    }
  }
}

void Output() {
  forall(person, people) {
    cout << "Person person" << *person << endl;
  }

  forall(state, states) {
    cout << "State state" << *state << endl;
  }
  
  forall(people, located) {
    cout << "Located state" << people->first
	 << " person" << people->second << endl;
  }

  forall(friends, friendship) {
    cout << "Friend " << friends->first << " " << friends->second << endl;
  }

  forall(college, colleges) {
    cout << "College " << *college
	 << " sz: " << c_size[*college]
	 << " diff: " << c_difficulty[*college] 
	 << " state: state" << located_map[*college] << endl;
  }

  forall (app, applications) {
    cout << "Applied person" << app->first
	 << " college" << app->second;
    if (acceptances.find(*app) != acceptances.end())
      cout << " ACCEPTED" << endl;
    else cout << " DENIED" << endl;

    if (attendance[app->first] == app->second) {
      cout << "Attended person" << app->first
	   <<" college" << app->second << endl;
    }
  }

}

int main(int argc, void ** argv) {
  srand ( time(NULL) );

  cout << RandDouble() << " " << RandDouble() << endl;

  // x states
  MakeStates(4);

  // x people
  MakePeople(150);

  // 1/x deleted from friends each step
  MakeFriends(6);

  // x colleges
  MakeColleges(30);

  // Pick a colleges for people
  // Person picks x colleges to apply to
  PickColleges(5);

  Output();
  return 0;
}
