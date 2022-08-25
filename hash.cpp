#include <map>

int main() {
  std::map<char, int> map;
  map['a'] = 1;
  map['b'] = 2;
  return map['a'];
}
