#ifndef ACCOUNT_H
#define ACCOUNT_H

#include "typedecl.h"
#include "db/fs.h"
#include "db/database.h"
#include "vector.h"

class Account {
  Bfsp bf;
  Account();
  sjtu::vector<username_t> login_list;

  bool logged_in(username_t user);
  bool exists(username_t user);

public:
  Database<username_t, user_profile> db;

  bool add_user(username_t cur, user_profile profile);
  bool login(username_t username, password_t password);
  bool logout(username_t username);
  user_profile query_profile(username_t cur, username_t user);
  user_profile modify_profile(username_t cur, username_t user, std::string gp, std::string gn, std::string gm, std::string gg);
  static Account &getInstance() {
    static Account instance;
    return instance;
  }
};

static Account &accounter = Account::getInstance();

#endif //ACCOUNT_H
