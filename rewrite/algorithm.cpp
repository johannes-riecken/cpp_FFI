#include <algorithm>
#include <numeric>
#include <vector>
#include <functional>
#include <cstdio>

#include <fmt/core.h>

auto my_lower_bound(auto f, auto l, auto val) {
    return std::equal_range(f, l, val).first;
}

extern "C" {
    int hs_lower_bound(int *arr0, int len0, int val) {
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = std::lower_bound(v0.begin(), v0.end(), val);
      return std::distance(v0.begin(), it);
    }

    int hs_my_lower_bound(int *arr0, int len0, int val) {
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = my_lower_bound(v0.begin(), v0.end(), val);
      return std::distance(v0.begin(), it);
    }

};
