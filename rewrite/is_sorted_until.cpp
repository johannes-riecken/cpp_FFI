extern "C" {
    int hs_is_sorted_until(int *arr, int len, int (*comp)(int, int)) {
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      auto it = std::is_sorted_until(v.begin(), v.end(), comp);
      return std::distance(v.begin(), it);
    }

    int hs_my_is_sorted_until(int *arr, int len, int (*comp)(int, int)) {
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      auto it = my_is_sorted_until(v.begin(), v.end(), comp);
      if (it != v.end()) {
        return std::distance(v.begin(), std::next(it));
      }
      return std::distance(v.begin(), it);
    }
}
