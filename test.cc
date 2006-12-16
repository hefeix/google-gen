#include <hash_map>
#include <hash_set>
#include <string> 
using namespace std;

class Foo{
  int hash(){
    return 17;
  }
};

int main(){
  hash_set<int> hsi;
  hash_set<string> hss;
  hash_set<Foo> hsf;

  hss.insert("a");
  hss.insert("b");
  for (hash_set<string>::iterator run = hss.begin(); run!=hss.end(); ++run){
    cout << *run << endl;
  }
  return 0;
}
