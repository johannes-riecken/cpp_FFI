#include <algorithm>
#include <numeric>
#include <vector>
#include <functional>
#include <cstdio>

#include <fmt/core.h>

auto arr_shift_left(auto f, auto l, auto val) {
    return std::copy(f + val, l, f);
}

extern "C" {
    void hs_shift_left(int *arr0, int len0, int val) {
      std::shift_left(arr0, arr0 + len0, val);
    }

    void hs_arr_shift_left(int *arr0, int len0, int val) {
      arr_shift_left(arr0, arr0 + len0, val);
    }

};
