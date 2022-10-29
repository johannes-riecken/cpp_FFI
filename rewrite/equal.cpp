extern "C" {
    bool hs_equal(int *arr0, int len0, int *arr1, int len1) {
      if (len1 < len0) {
        return false;
      }
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      std::vector<int> v1{};
      for (int i = 0; i < len1; i++) {
        v1.push_back(arr1[i]);
      }
      auto ret = std::equal(v0.begin(), v0.end(), v1.begin());
      return ret;
    }

    bool hs_my_equal(int *arr0, int len0, int *arr1, int len1) {
      if (len1 < len0) { // UB
        return false;
      }
      std::vector<int> v0{};
      for (int i = 0; i < len0; i++) {
        v0.push_back(arr0[i]);
      }
      std::vector<int> v1{};
      for (int i = 0; i < len1; i++) {
        v1.push_back(arr1[i]);
      }
      auto ret = my_equal(v0.begin(), v0.end(), v1.begin());
      return ret;
    }

}
