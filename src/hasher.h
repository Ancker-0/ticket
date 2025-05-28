#ifndef HASHER_H
#define HASHER_H

#include "db/util.h"

template <typename T>
struct Hasher;

template <size_t n>
struct Hasher<cstr<n>> {
  static const int B1 = 131;
  static const int MOD1 = 998244353;
  static const int B2 = 137;
  static const int MOD2 = 1e9 + 7;

  long long val;
  Hasher() = default;
  Hasher(const cstr<n> s) {
    int h1 = 0, h2 = 0;
    for (int i = 0; i <= n and s[i]; ++i) {
      h1 = ((long long)h1 * B1 + s[i]) % MOD1;
      h2 = ((long long)h2 * B2 + s[i]) % MOD2;
    }
    val = (long long)h1 << 32 | h2;
  }
  bool operator==(const Hasher &o) const { return val == o.val; }
  bool operator<(const Hasher &o) const { return val < o.val; }
};

#endif
