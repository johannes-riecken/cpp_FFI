#include <algorithm>
#include <vector>
#include <functional>
#include <cstdio>

#include <fmt/core.h>

auto my_adjacent_find(auto f, auto l, auto comp) {
    return std::mismatch(f, std::prev(l), std::next(f), std::not_fn(comp)).first;
}

extern "C" {
}

auto main() -> int {
  return 0;
}

