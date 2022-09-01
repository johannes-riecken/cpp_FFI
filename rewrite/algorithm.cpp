#include <algorithm>
#include <vector>
#include <functional>

#include <fmt/core.h>

auto my_adjacent_find(auto f, auto l, auto comp) {
    return std::mismatch(f, std::prev(l), std::next(f), comp).first;
}

auto my_is_sorted_until(auto f, auto l, auto comp = std::less_equal{}) {
    return std::adjacent_find(f, l, std::not_fn(comp));
}

auto my_is_sorted(auto f, auto l, auto comp = std::less_equal{}) {
    return std::is_sorted_until(f, l, comp) == l;
}

auto my_equal(auto f, auto l, auto f2) {
    return std::mismatch(f, l, f2, std::not_equal_to{}).first == l;
}

auto my_find_if(auto f, auto l, auto comp) {
    return std::mismatch(f, l, f, [&](auto a, auto b) {
        return comp(a);
    }).first;
}

auto my_any_of(auto f, auto l, auto comp) {
    return std::find_if(f, l, comp) != l;
}

auto my_find(auto f, auto l, auto val) {
    return std::find_if(f, l, [&](auto e) { return e == val; });
}

extern "C" {
    int hs_adjacent_find(int *arr, int len, int (*comp)(int, int)) {
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      return *std::adjacent_find(v.begin(), v.end(), comp);
    }

    int hs_my_adjacent_find(int *arr, int len, int (*comp)(int, int)) {
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      return *my_adjacent_find(v.begin(), v.end(), comp);
    }

    int hs_my_is_sorted_until(int *arr, int len, int (*comp)(int, int)) {
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      return *my_is_sorted_until(v.begin(), v.end(), comp);
    }

};


auto main() -> int {

    auto const v = std::vector{1, 1, 2, 3, 0};
    auto a = v.cbegin();
    auto b = v.cend();

    // same: 1
    fmt::print("{}\n", *my_adjacent_find(a, b, std::not_equal_to{}));
    fmt::print("{}\n", *std::adjacent_find(a, b, std::not_equal_to{}));
    __builtin_printf("\n");
    // TODO 3 vs 0
    fmt::print("{}\n", *my_is_sorted_until(a, b, std::less_equal{}));
    fmt::print("{}\n", *std::is_sorted_until(a, b));
    __builtin_printf("\n");
    // same: false
    fmt::print("{}\n", my_is_sorted(a, b, std::less_equal{}));
    fmt::print("{}\n", std::is_sorted(a, b));
    __builtin_printf("\n");
    // TODO false vs true
    fmt::print("{}\n", my_equal(a, b, a));
    fmt::print("{}\n", std::equal(a, b, a));
    __builtin_printf("\n");
    auto even = [](auto e) { return e % 2 == 0; };
    // TODO 1 vs 2
    fmt::print("{}\n", *my_find_if(a, b, even));
    fmt::print("{}\n", *std::find_if(a, b, even));
    __builtin_printf("\n");
    // same: true
    fmt::print("{}\n", my_any_of(a, b, even));
    fmt::print("{}\n", std::any_of(a, b, even));
    __builtin_printf("\n");
    // same: 3
    fmt::print("{}\n", *my_find(a, b, 3));
    fmt::print("{}\n", *std::find(a, b, 3));
}

