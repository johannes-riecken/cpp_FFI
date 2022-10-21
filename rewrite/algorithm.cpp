#include <algorithm>
#include <vector>
#include <functional>

#include <fmt/core.h>

auto my_equal(auto f, auto l, auto f2) {
    return std::mismatch(f, l, f2, std::not_equal_to{}).first == l;
}

extern "C" {
};


auto main() -> int {
  return 0;
}

