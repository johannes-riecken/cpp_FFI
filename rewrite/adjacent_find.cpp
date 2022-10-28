extern "C" {
    int hs_adjacent_find(int *arr, int len, int (*comp)(int, int)) {
      if (len < 2) {
        return 0;
      }
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      auto it = std::adjacent_find(v.begin(), v.end(), comp);
      if (it == v.end()) {
        return 0;
      }
      return std::distance(v.begin(), it);
    }

    int hs_my_adjacent_find(int *arr, int len, int (*comp)(int, int)) {
      if (len < 2) {
        return 0;
      }
      std::vector<int> v{};
      for (int i = 0; i < len; i++) {
        v.push_back(arr[i]);
      }
      auto it = my_adjacent_find(v.begin(), v.end(), comp);
      if (it < v.begin() || it == std::prev(v.end())) {
        return 0;
      }
      return std::distance(v.begin(), it);
    }

}
