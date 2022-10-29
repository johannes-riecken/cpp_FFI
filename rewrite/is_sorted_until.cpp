extern "C" {
    int hs_is_sorted_until(int *arr0, int len0, int (*comp)(int, int)) {
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = std::is_sorted_until(v0.begin(), v0.end(), comp);
      return std::distance(v0.begin(), it);
    }

    int hs_my_is_sorted_until(int *arr0, int len0, int (*comp)(int, int)) {
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = my_is_sorted_until(v0.begin(), v0.end(), comp);
      if (it != v0.end()) {
        return std::distance(v0.begin(), std::next(it));
      }
      return std::distance(v0.begin(), it);
    }
}
