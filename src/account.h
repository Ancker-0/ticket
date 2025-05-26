#ifndef ACCOUNT_H
#define ACCOUNT_H

#include "typedecl.h"
#include "db/fs.h"
#include "db/database.h"
#include "vector.h"

// TODO: int may not fit the price
struct invoice_t {
  trainID_t trainID;
  username_t username;
  int status;  // 1 for success, 0 for pending, -1 for refunded
  date_t realdate;
  seatNum_t num;
  int price;

  stationName_t from, to;
  time_and_date_t leaving_time, arriving_time;
  int invoice_id;
};

class Account {
  Bfsp bf;
  fs_vector vec_ass;
  Account();
  sjtu::vector<username_t> login_list;

  bool logged_in(username_t user);
  bool exists(username_t user);
  invoice_t create_invoice(username_t user, invoice_t invoice);

public:
  Database<username_t, user_profile> db;

  bool add_user(username_t cur, user_profile profile);
  bool login(username_t username, password_t password);
  bool logout(username_t username);
  user_profile query_profile(username_t cur, username_t user);
  user_profile modify_profile(username_t cur, username_t user, std::string gp, std::string gn, std::string gm, std::string gg);
  std::pair<int, int> buy_ticket(username_t user, trainID_t trainID, date_t date, stationName_t start, stationName_t end, seatNum_t n, bool queue);
  sjtu::vector<invoice_t> query_order(username_t user);
  bool refund(username_t user, int order_id);

  static Account &getInstance() {
    static Account instance;
    return instance;
  }
  invoice_t get_invoice(username_t user, int order_id);
};

static Account &accounter = Account::getInstance();

#endif //ACCOUNT_H
