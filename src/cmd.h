#ifndef CMD_H
#define CMD_H

#include <cassert>
#include <iostream>
#include <string>

#include "account.h"
#include "trainer.h"
#include "typedecl.h"
#include "vector.h"
// #include <vector>
// namespace sjtu {
//   template <typename T> using vector = std::vector<T>;
// };

std::string add_user_handler(const std::string &gc, const std::string &gu,
                             const std::string &gp, const std::string &gn,
                             const std::string &gm, const std::string &gg) {
  // TODO: no check -c and -g for the first user
  if (not((([](std::string) { return true; }(gc)) &&
           ([](std::string) { return true; }(gu)) &&
           ([](std::string) { return true; }(gp)) &&
           ([](std::string) { return true; }(gn)) &&
           ([](std::string) { return true; }(gm)) &&
           ([](std::string) { return true; }(gg)))))
    return "-1";
  return accounter.add_user(username_converter(gc),
                            user_profile(gu, gp, gn, gm, gg))
             ? "0"
             : "-1";
}

std::string login_handler(const std::string &gu, const std::string &gp) {
  if (not((([](std::string) { return true; }(gu)) &&
           ([](std::string) { return true; }(gp)))))
    return "-1";
  return accounter.login(username_converter(gu), password_converter(gp)) ? "0"
                                                                         : "-1";
}

std::string logout_handler(const std::string &gu) {
  if (not((([](std::string) { return true; }(gu))))) return "-1";
  return accounter.logout(username_converter(gu)) ? "0" : "-1";
}

std::string query_profile_handler(const std::string &gc,
                                  const std::string &gu) {
  if (not((([](std::string) { return true; }(gc)) &&
           ([](std::string) { return true; }(gu)))))
    return "-1";
  try {
    user_profile prof =
        accounter.query_profile(username_converter(gc), username_converter(gu));
    if (gc == "Savage" and gu == "Catapult")
      std::cerr << "! " << (std::string)accounter.db.get(username_converter(gc))
                << '\n'
                << (std::string)accounter.db.get(username_converter(gu))
                << std::endl;
    return (std::string)prof;
  } catch (const Error &e) {
    return "-1";
  }
  return assert(false), "-1";
}

std::string modify_profile_handler(const std::string &gc, const std::string &gu,
                                   const std::string &gp, const std::string &gn,
                                   const std::string &gm,
                                   const std::string &gg) {
  if (not((([](std::string) { return true; }(gc)) &&
           ([](std::string) { return true; }(gu)))))
    return "-1";
  if (not(((((((gp) == (""))) || ([](std::string) { return true; }(gp)))) &&
           (((((gn) == (""))) || ([](std::string) { return true; }(gn)))) &&
           (((((gm) == (""))) || ([](std::string) { return true; }(gm)))) &&
           (((((gg) == (""))) || ([](std::string) { return true; }(gg)))))))
    return "-1";
  try {
    user_profile prof = accounter.modify_profile(
        username_converter(gc), username_converter(gu), gp, gn, gm, gg);
    return (std::string)prof.username + " " + (std::string)prof.realname + " " +
           (std::string)prof.mail + " " + number2string(prof.privilege);
  } catch (const Error &) {
    return "-1";
  }
  return assert(false), "-1";
}

std::string add_train_handler(const std::string &gi, const std::string &gn,
                              const std::string &gm, const std::string &gs,
                              const std::string &gp, const std::string &gx,
                              const std::string &gt, const std::string &go,
                              const std::string &gd, const std::string &gy) {
  train_t train(gi, gn, gs, gm, gp, gx, gt, go, gd, gy);
  return trainer.add_train(train) ? "0" : "-1";
}

std::string delete_train_handler(const std::string &gi) {
  return trainer.delete_train(trainID_converter(gi)) ? "0" : "-1";
}

std::string release_train_handler(const std::string &gi) {
  return trainer.release_train(trainID_converter(gi)) ? "0" : "-1";
}

std::string query_train_handler(const std::string &gi, const std::string &gd) {
  try {
    train_t train = trainer.query_train(trainID_converter(gi));
    std::string res;
    res += std::string(train.trainID) + " " + train.train_type + "\n";
    price_t price = 0;
    if (![](std::string) { return true; }(gd)) return "-1";
    date_t date = date_converter(gd);
    if (!date_range(date, train.saleDate[0], train.saleDate[1])) return "-1";
    for (int i = 0; i < train.stationNum; ++i) {
      if (i) {
        res += "\n";
        price += train.prices[i - 1];
      }
      // TODO: improve time calculation
      auto tmp1 = train.arrive_time(train.stationNames[i]);
      auto tmp2 = train.leaving_time(train.stationNames[i]);
      tmp1.first += date;
      tmp2.first += date;
      res += std::string(train.stationNames[i]) + " " +
             std::string(
                 i == 0 ? "xx-xx xx:xx"
                        : time_and_date_printer(tmp1) /* arriving time */) +
             " -> " +
             std::string(i + 1 == train.stationNum
                             ? "xx-xx xx:xx"
                             : time_and_date_printer(tmp2) /* leaving time */) +
             " " + number2string(price) + " " +
             ((i + 1 < train.stationNum)
                  ? number2string(train.seatNum - getSeat(train.seat[date][i]))
                  : "x");
    }
    return res;
  } catch (const Error &) {
    return "-1";
  }
  return assert(false), "42";
}

std::string query_ticket_handler(const std::string &gs, const std::string &gt,
                                 const std::string &gd, const std::string &gp) {
  assert(gp == "" or gp == "time" or gp == "cost");
  sjtu::vector<Trainer::qry_ticket_t> vec =
      trainer.query_ticket(date_converter(gd), stationName_converter(gs),
                           stationName_converter(gt), gp == "cost");
  std::string ret;
  ret += number2string(vec.size());
  for (int i = 0; i < (int)vec.size(); ++i) {
    ret += '\n';
    ret += std::string(vec[i].trainID) + " " + gs + " " +
           time_and_date_printer(vec[i].leaving_time) + " -> " + gt + " " +
           time_and_date_printer(vec[i].arriving_time) + " " +
           number2string(vec[i].price) + " " + number2string(vec[i].seat);
  }
  return ret;
}

std::string query_transfer_handler(const std::string &gs, const std::string &gt,
                                   const std::string &gd,
                                   const std::string &gp) {
  Trainer::transfer_t vec =
      trainer.query_transfer(date_converter(gd), stationName_converter(gs),
                             stationName_converter(gt), gp == "cost");
  if (vec.transferName == "") return "0";
  std::string ret;
  ret += std::string(vec.q1.trainID) + " " + gs + " " +
         time_and_date_printer(vec.q1.leaving_time) + " -> " +
         std::string(vec.transferName) + " " +
         time_and_date_printer(vec.q1.arriving_time) + " " +
         number2string(vec.q1.price) + " " + number2string(vec.q1.seat);
  ret += '\n';
  ret += std::string(vec.q2.trainID) + " " + std::string(vec.transferName) +
         " " + time_and_date_printer(vec.q2.leaving_time) + " -> " + gt + " " +
         time_and_date_printer(vec.q2.arriving_time) + " " +
         number2string(vec.q2.price) + " " + number2string(vec.q2.seat);
  return ret;
}

std::string buy_ticket_handler(const std::string &gu, const std::string &gi,
                               const std::string &gd, const std::string &gn,
                               const std::string &gf, const std::string &gt,
                               const std::string &gq) {
  auto ret = accounter.buy_ticket(username_converter(gu), trainID_converter(gi),
                                  date_converter(gd), stationName_converter(gf),
                                  stationName_converter(gt),
                                  string2non_negative(gn), gq == "true");
  return ret.first == 1   ? number2string(ret.second)
         : ret.first == 0 ? "queue"
                          : "-1";
}

std::string query_order_handler(const std::string &gu) {
  try {
    sjtu::vector<invoice_t> vec = accounter.query_order(username_converter(gu));
    std::string ret = number2string(vec.size());
    for (int i = (int)vec.size() - 1; i >= 0; --i) {
      ret += '\n';
      ret += std::string(vec[i].status == 1    ? "[success]"
                         : vec[i].status == 0  ? "[pending]"
                         : vec[i].status == -1 ? "[refunded]"
                                               : "UNKNOWN") +
             " " + std::string(vec[i].trainID) + " " +
             std::string(vec[i].from) + " " +
             time_and_date_printer(vec[i].leaving_time) + " -> " +
             std::string(vec[i].to) + " " +
             time_and_date_printer(vec[i].arriving_time) + " " +
             number2string(vec[i].price) + " " + number2string(vec[i].num);
    }
    return ret;
  } catch (const Error &) {
    return "-1";
  }
  return assert(false), "42";
}

std::string refund_ticket_handler(const std::string &gu,
                                  const std::string &gn) {
  return accounter.refund(username_converter(gu),
                          gn == "" ? 1 : string2non_negative(gn))
             ? "0"
             : "-1";
}

std::string clean_handler() {
  assert(false);
  return "sorry, not implemented!";
}

std::string exit_handler() { return "bye"; }

std::string dispatch(std::string line) {
  long long time = 0;

#if WEAK_TIME
  int p = 0;
  if (line[0] == '[') {
    assert(line.size() and line[0] == '[');
    p = 1;
    for (; p < (int)line.size(); ++p) {
      if ('0' <= line[p] and line[p] <= '9')
        time = time * 10 + line[p] - '0';
      else if (line[p] == ']')
        break;
      else
        assert(false);
    }
    assert(p + 1 < (int)line.size() and line[p] == ']' and line[p + 1] == ' ');
    ++p;
  } else
    time = -1;
#else
  assert(line.size() and line[0] == '[');
  int p = 1;
  for (; p < (int)line.size(); ++p) {
    if ('0' <= line[p] and line[p] <= '9')
      time = time * 10 + line[p] - '0';
    else if (line[p] == ']')
      break;
    else
      assert(false);
  }
  assert(p + 1 < (int)line.size() and line[p] == ']' and line[p + 1] == ' ');
  ++p;
#endif

  std::string time_str = "[" + number2string(time) + "] ";

  while (p < (int)line.size() and line[p] == ' ') ++p;
  std::string cmd = ({
    std::string tmp;
    while (p < (int)line.size() and line[p] == ' ') ++p;
    while (!(p >= (int)line.size()) and line[p] != ' ') {
      tmp += line[p++];
    }
    tmp;
  });

  sjtu::vector<std::pair<std::string, std::string>> kvpair;
  while (p < (int)line.size()) {
    while (p < (int)line.size() and line[p] == ' ') ++p;
    if ((p >= (int)line.size())) break;
    assert(p + 2 < (int)line.size() and line[p] == '-' and line[p + 2] == ' ');
    std::string k(1, line[p + 1]), v = "";
    p += 2;
    while (p < (int)line.size() and line[p] == ' ') ++p;
    while (!(p >= (int)line.size()) and line[p] != ' ') v += line[p++];
    kvpair.push_back({k, v});
  }

  // for (auto [k, v] : kvpair)
  //   printf("%s => %s\n", k.data(), v.data());

  // sjtu::vector<std::string> values;
  // for (auto &[k, v] : kvpair)
  //   if (k == "a")
  //     values.push_back(v);
  // assert(values.size() <= 1);
  // arguments.push_back(values[0]);

  if (((cmd) == ("exit"))) {
    sjtu::vector<std::string> arguments;
    return time_str + exit_handler();
  } else if (((cmd) == ("clean"))) {
    sjtu::vector<std::string> arguments;
    return time_str + clean_handler();
  } else if (((cmd) == ("refund_ticket"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "n") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    return time_str + refund_ticket_handler(arguments[0], arguments[1]);
  } else if (((cmd) == ("query_order"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + query_order_handler(arguments[0]);
  } else if (((cmd) == ("buy_ticket"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "i") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "d") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "n") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "f") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "t") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "q") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    return time_str + buy_ticket_handler(arguments[0], arguments[1],
                                         arguments[2], arguments[3],
                                         arguments[4], arguments[5],
                                         arguments[6]);
  } else if (((cmd) == ("query_transfer"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "s") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "t") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "d") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    return time_str + query_transfer_handler(arguments[0], arguments[1],
                                             arguments[2], arguments[3]);
  } else if (((cmd) == ("query_ticket"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "s") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "t") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "d") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    return time_str + query_ticket_handler(arguments[0], arguments[1],
                                           arguments[2], arguments[3]);
  } else if (((cmd) == ("query_train"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "i") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "d") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + query_train_handler(arguments[0], arguments[1]);
  } else if (((cmd) == ("release_train"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "i") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + release_train_handler(arguments[0]);
  } else if (((cmd) == ("delete_train"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "i") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + delete_train_handler(arguments[0]);
  } else if (((cmd) == ("add_train"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "i") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "n") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "m") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "s") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "x") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "t") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "o") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "d") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "y") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + add_train_handler(arguments[0], arguments[1],
                                        arguments[2], arguments[3],
                                        arguments[4], arguments[5],
                                        arguments[6], arguments[7],
                                        arguments[8], arguments[9]);
  } else if (((cmd) == ("modify_profile"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "c") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "n") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "m") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "g") values.push_back(v);
      assert(values.size() <= 1);
      if (!values.empty())
        arguments.push_back(values[0]);
      else
        arguments.push_back("");
    }
    return time_str + modify_profile_handler(arguments[0], arguments[1],
                                             arguments[2], arguments[3],
                                             arguments[4], arguments[5]);
  } else if (((cmd) == ("query_profile"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "c") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + query_profile_handler(arguments[0], arguments[1]);
  } else if (((cmd) == ("logout"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + logout_handler(arguments[0]);
  } else if (((cmd) == ("login"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + login_handler(arguments[0], arguments[1]);
  } else if (((cmd) == ("add_user"))) {
    sjtu::vector<std::string> arguments;
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "c") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "u") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "p") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "n") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "m") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    {
      sjtu::vector<std::string> values;
      for (auto &[k, v] : kvpair)
        if (k == "g") values.push_back(v);
      assert(values.size() == 1);
      arguments.push_back(values[0]);
    }
    return time_str + add_user_handler(arguments[0], arguments[1], arguments[2],
                                       arguments[3], arguments[4],
                                       arguments[5]);
  }

  return time_str + "unrecognized command!";
}
#endif
