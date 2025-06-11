#ifndef TYPEDECL_H
#define TYPEDECL_H

#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>

#include "db/fs_vector.h"
#include "db/util.h"

const auto username_checker = [](std::string) { return true; };

const auto password_checker = [](std::string) { return true; };

static bool realname_checker(std::string s) {
  // TODO: check UTF8 hans character
  return 2 <= s.length() and s.length() <= 20 and true;
}

const auto mail_checker = [](std::string) { return true; };

static bool privilege_checker(std::string s) {
  int x = string2non_negative(s);
  return 0 <= x and x <= 10;
}

const auto trainID_checker = [](std::string) { return true; };

const auto seatNum_checker = [](std::string) { return true; };

const auto stationName_checker = [](std::string) { return true; };

const auto transferCode_checker = [](std::string) { return true; };

// #define DF(n, len) using n##_t = cstr<len>;

using username_t = cstr<20>;

static username_t username_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<20>(s);
}
using password_t = cstr<30>;

static password_t password_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<30>(s);
}
using realname_t = cstr<15>;

static realname_t realname_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<15>(s);
}
using mail_t = cstr<30>;

static mail_t mail_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<30>(s);
}
using trainID_t = cstr<20>;

static trainID_t trainID_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<20>(s);
}
using stationName_t = cstr<30>;

static stationName_t stationName_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<30>(s);
}
using transferCode_t = cstr<80>;

static transferCode_t transferCode_converter(std::string s) {
  assert([](std::string) { return true; }(s));
  return string2cstr<80>(s);
}

using privilege_t = int;
using stationNum_t = int;
using Time_t = int;
using price_t = int;
using duration_t = int;
using seatNum_t = int;
using date_t = __int8_t;
using train_type_t = char;
using time_and_date_t = std::pair<date_t, Time_t>;
using seatNeat = sjtu::array<unsigned char, 3>;

static seatNum_t getSeat(seatNeat x) {
  return (int)x[0] << 16 | (int)x[1] << 8 | (int)x[2];
}

static seatNeat putSeat(seatNum_t x) {
  seatNeat ret;
  ret[0] = (x >> 16) & 0xFF;
  ret[1] = (x >> 8) & 0xFF;
  ret[2] = x & 0xFF;
  return ret;
}

#define WITH_SEAT(value, dull, expr) \
  ({                                 \
    int dull = getSeat(value);       \
    expr;                            \
    value = putSeat(dull);           \
  })

static std::string date_printer(date_t d) {
  // Why there is a train running up to September...
  int mm = d < 30 ? 6 : d < 61 ? 7 : d < 92 ? 8 : d < 122 ? 9 : -1;
  int dd = 1 + (d < 30    ? d
                : d < 61  ? d - 30
                : d < 92  ? d - 61
                : d < 122 ? d - 92
                          : -1);
  static char buf[20];
  sprintf(buf, "%02d-%02d", mm, dd);
  return std::string(buf);
}

static std::string time_printer(Time_t d) {
  int hh = d / 60;
  int mm = d % 60;
  static char buf[20];
  sprintf(buf, "%02d:%02d", hh, mm);
  return std::string(buf);
}

static std::string time_and_date_printer(const time_and_date_t &x) {
  return date_printer(x.first) + " " + time_printer(x.second);
}

inline static int time_and_date_diff(const time_and_date_t &s,
                                     const time_and_date_t &t) {
  return (t.first - s.first) * 1440 + (t.second - s.second);
}

static time_and_date_t time_and_date_advance(time_and_date_t t,
                                             duration_t delta) {
  int min = t.second + delta;
  t.second = min % 1440;
  t.first += min / 1440;
  return t;
}

static bool time_and_date_less(time_and_date_t x, time_and_date_t y) {
  return x.first == y.first ? x.second < y.second : x.first < y.first;
}

const Time_t invalid_time = -1;
static Time_t Time_converter(std::string s) {
  if (not(s.size() == 5 and s[2] == ':')) return invalid_time;
  for (int i = 0; i < 5; ++i)
    if (i != 2 and not char_in(s[i], "0123456789")) return invalid_time;
  return (s[0] - '0') * 600 + (s[1] - '0') * 60 + (s[3] - '0') * 10 +
         (s[4] - '0');
}

const auto Time_checker =
    ([](std::string s) { return (Time_converter(s) != invalid_time); });

const date_t invalid_date = -1;  // 0xFFFF
static date_t date_converter(std::string s) {
  if (not(s.size() == 5 and s[2] == '-')) return invalid_date;
  for (int i = 0; i < 5; ++i)
    if (i != 2 and not char_in(s[i], "0123456789")) return invalid_date;
  date_t mm = (s[0] - '0') * 10 + s[1] - '0';
  date_t dd = (s[3] - '0') * 10 + s[4] - '0';
  if (not(((mm == 6 and dd <= 30) || (mm == 7 and dd <= 31) ||
           (mm == 8 and dd <= 31))))
    return invalid_date;
  return dd - 1 + (mm >= 7) * 30 + (mm >= 8) * 31;
}
const auto date_checker =
    ([](std::string s) { return date_converter(s) != invalid_date; });

struct user_profile {
  username_t username;
  password_t password;
  realname_t realname;
  mail_t mail;
  privilege_t privilege;

  fs_vector::Head invoices;

  user_profile() = default;
  user_profile(std::string s0, std::string s1, std::string s2, std::string s3,
               std::string s4) {
    username = username_converter(s0);
    password = password_converter(s1);
    realname = realname_converter(s2);
    mail = mail_converter(s3);
    privilege = string2non_negative(s4);
    invoices = fs_vector::Head();
  }
  explicit operator std::string() {
    return (std::string)username + " " + (std::string)realname + " " +
           (std::string)mail + " " + number2string(privilege);
  }
};

struct train_t {
  // original field
  trainID_t trainID;
  stationNum_t stationNum;
  sjtu::array<stationName_t, 100> stationNames;
  seatNum_t seatNum;
  sjtu::array<price_t, 99> prices;
  Time_t startTime;
  sjtu::array<duration_t, 99> travelTimes;
  sjtu::array<duration_t, 98> stopoverTimes;
  date_t saleDate[2];
  train_type_t train_type;

  // additional field
  bool released;
  sjtu::array<sjtu::array<seatNeat, 99>, 92> seat;
  fs_vector::Head queue;
  int queue_head;

  train_t() = default;

  train_t(std::string s0, std::string s1, std::string s2, std::string s3,
          std::string s4, std::string s5, std::string s6, std::string s7,
          std::string s8, std::string s9) {
    trainID = trainID_converter(s0);
    stationNum = string2non_negative(s1);
    {
      auto tmp = split(s2);
      for (size_t i = 0; i < tmp.size(); ++i)
        stationNames[i] = stationName_converter(tmp[i]);
    }
    seatNum = string2non_negative(s3);
    {
      auto tmp = split(s4);
      for (size_t i = 0; i < tmp.size(); ++i)
        prices[i] = string2non_negative(tmp[i]);
    }
    startTime = Time_converter(s5);
    {
      auto tmp = split(s6);
      for (size_t i = 0; i < tmp.size(); ++i)
        travelTimes[i] = string2non_negative(tmp[i]);
    }
    {
      auto tmp = split(s7);
      for (size_t i = 0; i < tmp.size(); ++i)
        stopoverTimes[i] = string2non_negative(tmp[i]);
    }
    {
      auto tmp = split(s8);
      for (size_t i = 0; i < tmp.size(); ++i)
        saleDate[i] = date_converter(tmp[i]);
    }
    train_type = ([](std::string s) { return s[0]; })(s9);

    seat = decltype(seat)();
    released = false;
    queue = fs_vector::Head{};
    queue_head = 0;
  }

  int get_station_id(stationName_t name) const {
    for (int i = 0; i < stationNum; ++i)
      if (stationNames[i] == name) return i;
    return -1;
  }

  time_and_date_t arrive_time(int p) const {
    assert(~p);
    time_and_date_t ret = {0, startTime};
    for (int i = 0; i < p; ++i) {
      ret = time_and_date_advance(ret, travelTimes[i]);
      if (i > 0 and i < p)
        ret = time_and_date_advance(ret, stopoverTimes[i - 1]);
    }
    return ret;
  }

  time_and_date_t arrive_time(stationName_t name) const {
    return arrive_time(get_station_id(name));
  }

  time_and_date_t leaving_time(int p) const {
    assert(~p);
    time_and_date_t ret = arrive_time(p);
    return p ? time_and_date_advance(ret, stopoverTimes[p - 1]) : ret;
  }

  time_and_date_t leaving_time(stationName_t name) const {
    return leaving_time(get_station_id(name));
  }
};

inline static bool date_range(date_t x, date_t s, date_t t) {
  return s <= x and x <= t;
}

#endif
