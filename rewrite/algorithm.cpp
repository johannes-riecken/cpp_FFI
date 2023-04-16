#include <algorithm>
#include <numeric>
#include <vector>
#include <functional>
#include <cstdio>

auto arr_exclusive_scan(auto f, auto l, auto f2, auto val) {
    std::inclusive_scan(f, l, f2);
}

extern "C" {
    void hs_exclusive_scan(int *arr0, int len0, int *arr1, int val0) {
      std::exclusive_scan(arr0, arr0 + len0, arr1, val0);
    }

    void hs_arr_exclusive_scan(int *arr0, int len0, int *arr1, int val0) {
      arr_exclusive_scan(arr0, arr0 + len0, arr1, val0);
    }

};
