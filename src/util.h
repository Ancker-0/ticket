#ifndef UTIL_H
#define UTIL_H

#include "error.h"

#include <array>
#include <cmath>
#include <sstream>
#include <cstring>
#include <cstdint>

template <size_t size>
class cstr : public std::array<char, size + 1> {
public:
  static constexpr int N = size;
};

typedef int64_t pos_t;

// book types
#define DF(n, len) using n##_t = cstr<len>;
DF(ISBN, 20);
DF(bookname, 60);
DF(author, 60);
DF(keyword, 60);
using quantity_t = uint32_t;
using price_t = double;
using totalcost_t = double;
using bookid_t = pos_t;

// account types
using privilege_t = int;
using identity_t = cstr<10>;
using userid_t = cstr<30>;
using username_t = cstr<30>;
using password_t = cstr<30>;

#endif //UTIL_H