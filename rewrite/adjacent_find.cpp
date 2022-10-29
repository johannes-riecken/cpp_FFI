extern "C" {
    int hs_adjacent_find(int *arr0, int len0, int (*comp)(int, int)) {
      if (len0 < 1) {
        return 0;
      }
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = std::adjacent_find(v0.begin(), v0.end(), comp);
      return std::distance(v0.begin(), it);
    }

    int hs_my_adjacent_find(int *arr0, int len0, int (*comp)(int, int)) {
      if (len0 < 1) {
        return 0;
      }
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      auto it = my_adjacent_find(v0.begin(), v0.end(), comp);
      if (it == std::prev(v0.end())) {
        return len0;
      }
      return std::distance(v0.begin(), it);
    }

}
