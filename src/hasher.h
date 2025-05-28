#include "db/util.h"

template <typename T>
struct Hasher;

template <size_t n>
struct Hasher<cstr<n>> {
  static const int B = 131;
  static const int MOD = 998244353;
  int val;
  Hasher() = default;
  Hasher(const cstr<n> s) {
    val = 0;
    for (int i = 0; i <= n and s[i]; ++i)
      val = (val * B + s[i]) % MOD;
  }
  bool operator==(const Hasher &o) const { return val == o.val; }
  bool operator<(const Hasher &o) const { return val < o.val; }
};
