#ifndef UTIL_H
#define UTIL_H

#include "../vector.h"
#include "error.h"

#include <cstring>

const int INF = 0x3F3F3F3F;

namespace sjtu {
template <typename T, size_t size>
class array {
  T arr[size];
public:
  array() = default;
  array &operator=(const array<T, size> &o) {
    for (size_t i = 0; i < size; ++i)
      arr[i] = o.arr[i];
    return *this;
  }
  T &operator[](size_t idx) { return arr[idx]; }
  const T &operator[](size_t idx) const { return arr[idx]; }
  T *data() { return arr; }
  const T *data() const { return arr; }
};
}

template <size_t size>
class cstr : public sjtu::array<char, size + 1> {
public:
  static constexpr int N = size;

  explicit operator std::string() const {
    return sjtu::array<char, size + 1>::data();
  }
};

typedef int64_t pos_t;

// book types
// #define DF(n, len) using n##_t = cstr<len>;
// DF(ISBN, 20);
// DF(bookname, 60);
// DF(author, 60);
// DF(keyword, 60);

static const char userid_chars[] = "1234567890zxcvbnmasdfghjklqwertyuiopZXCVBNMASDFGHJKLQWERTYUIOP_";
static const char username_chars[] = " 0@P`p!1AQaq\"2BRbr#3CScs$4DTdt%5EUeu&6FVfv'7GWgw(8HXhx)9IYiy*:JZjz+;K[k{,<L\\l|-=M]m}.>N^n~/?O_o";  // also used by ISBN
static const char bookname_chars[] = " 0@P`p!1AQaq2BRbr#3CScs$4DTdt%5EUeu&6FVfv'7GWgw(8HXhx)9IYiy*:JZjz+;K[k{,<L\\l|-=M]m}.>N^n~/?O_o";

static bool inside(char c, const char *s) {
  for (; *s; ++s)
    if (c == *s)
      return true;
  return false;
}

template <size_t size>
bool operator==(const cstr<size> &u, const std::string &v) {
  return u.data() == v;
}

template <size_t size>
bool operator==(const std::string &v, const cstr<size> &u) {
  return u.data() == v;
}

template <size_t sizeu, size_t sizev>
bool operator==(const cstr<sizeu> &u, const cstr<sizev> &v) {
  return strcmp(u.data(), v.data()) == 0;
}

template <size_t sizeu, size_t sizev>
bool operator!=(const cstr<sizeu> &u, const cstr<sizev> &v) {
  return not (u == v);
}  // TODO: does C++ derive operator!= from operator== ?

template <size_t sizeu, size_t sizev>
bool operator<(const cstr<sizeu> &u, const cstr<sizev> &v) {
  return strcmp(u.data(), v.data()) < 0;
}

template <size_t size>
static cstr<size> string2cstr(std::string s) {
  // Massert(s.size() <= size, "cstr cannot fit");
  cstr<size> ret;
  strncpy(ret.data(), s.c_str(), size + 1);
  return ret;
}

static std::string number2string(long long n) {
  static char buf[64];
  sprintf(buf, "%lld", n);
  return buf;
}

static int string2non_negative(std::string s) {
  // return std::atoi(s.c_str());
  Massert(s.size() > 0, "empty string");
  Massert(s.size() <= 10, "too large");
  long long ret = 0;
  for (int i = 0; i < (int)s.size(); ++i) {
    Massert('0' <= s[i] and s[i] <= '9', "not number");
    ret = ret * 10 + s[i] - '0';
  }
  Massert(-2147483648 <= ret and ret <= 2147483647, "too large");
  return (int)ret;
}

static sjtu::vector<std::string> split(std::string s) {
  sjtu::vector<std::string> ret;
  std::string buf;
  for (char c : s)
    if (c == '|')
      ret.push_back(buf), buf = "";
    else {
      assert(c != ' ');
      buf += c;
    }
  ret.push_back(buf);
  return ret;
}

static bool char_in(const char &ch, const std::string &rng) {
  for (const auto &c : rng)
    if (c == ch)
      return true;
  return false;
}

#endif //UTIL_H
