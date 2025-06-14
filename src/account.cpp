#include "account.h"

#include "trainer.h"

Account::Account() : bf("account.db"), vec_ass(bf), db(bf) {}

bool Account::logged_in(username_t user) {
  for (auto &s : login_list)
    if (user == s) return true;
  return false;
}

bool Account::exists(username_t user) { return db.exist(user); }

bool Account::refund(username_t user, int order_id) {
  if (!logged_in(user)) return false;
  user_profile up = db.get(user);
  sjtu::vector<invoice_t> invs = vec_ass.getAll<invoice_t>(up.invoices);
  if (order_id > invs.size()) return false;
  order_id = invs.size() - order_id;
  invoice_t &inv = invs[order_id];
  if (inv.status == -1) return false;
  if (inv.status == 1)
    trainer.refund_ticket(inv.trainID, inv.realdate, inv.from, inv.to, inv.num);
  inv.status = -1;
  vec_ass.update(up.invoices, order_id, inv);
  db.modify(user, up);
  return true;
}

std::pair<int, int> Account::buy_ticket(username_t user, trainID_t trainID,
                                        date_t date, stationName_t start,
                                        stationName_t end, seatNum_t n,
                                        bool queue) {
  static const std::pair<int, int> fail = {-1, -1};
  if (!logged_in(user)) return fail;
  train_t train = trainer.query_train(trainID);
  if (!train.released or train.seatNum < n) return fail;
  int ps = train.get_station_id(start), pt = train.get_station_id(end);
  if (ps == -1 or pt == -1 or ps >= pt) return fail;
  seatNum_t seat = 0;
  time_and_date_t leaving_time = train.leaving_time(start),
                  arriving_time = train.arrive_time(end);
  date_t realdate = date - leaving_time.first;
  leaving_time.first += realdate;
  arriving_time.first += realdate;
  if (!date_range(realdate, train.saleDate[0], train.saleDate[1])) return fail;
  int price = 0;
  for (int i = ps; i < pt; ++i) {
    seat = std::max(seat, getSeat(train.seat[realdate][i]));
    price += train.prices[i];
  }
  seat = train.seatNum - seat;
  if (seat >= n) {
    for (int i = ps; i < pt; ++i) WITH_SEAT(train.seat[realdate][i], x, x += n);
    trainer.update_train(train);
    create_invoice(user, invoice_t{train.trainID, user, 1, realdate, n, price,
                                   start, end, leaving_time, arriving_time});
    return {1, price * n};
  } else if (queue) {
    trainer.pend(create_invoice(
        user, invoice_t{train.trainID, user, 0, realdate, n, price, start, end,
                        leaving_time, arriving_time}));
    return {0, -1};
  } else
    return {-1, -1};
}

sjtu::vector<invoice_t> Account::query_order(username_t user) {
  if (!logged_in(user)) throw Error("not logged in");
  return vec_ass.getAll<invoice_t>(db.get(user).invoices);
}

invoice_t Account::create_invoice(username_t user, invoice_t invoice) {
  user_profile up = db.get(user);
  invoice.invoice_id = up.invoices.size;
  vec_ass.push_back(up.invoices, invoice);
  db.modify(user, up);
  return invoice;
}

bool Account::add_user(username_t cur, user_profile profile) {
  bool init;
  bf.getHeaderT(1, init);
  if (!init) {
    init = true;
    profile.privilege = 10;
    db.insert(profile.username, profile);
    bf.putT(bf.getHeaderPos(1), init);
    return true;
  }
  try {
    user_profile cur_profile = db.get(cur);
    if (!logged_in(cur) or exists(profile.username) or
        cur_profile.privilege <= profile.privilege)
      return false;
    db.insert(profile.username, profile);
    return true;
  } catch (const Error &) {
    return false;
  }
  return false;
}

bool Account::login(username_t username, password_t password) {
  if (logged_in(username) or !exists(username)) return false;
  user_profile prof = db.get(username);
  if (password != prof.password) return false;
  login_list.push_back(username);
  return true;
}

bool Account::logout(username_t username) {
  if (!logged_in(username)) return false;
  for (size_t i = 0; i < login_list.size(); ++i)
    if (login_list[i] == username) {
      login_list.erase(login_list.begin() + i);
      return true;
    }
  return assert(false), false;
}

user_profile Account::query_profile(username_t cur, username_t user) {
  if (!logged_in(cur)) throw Error("user not logged in");
  if (!exists(user)) throw Error("user does not exist");
  user_profile cur_p = db.get(cur), user_p = db.get(user);
  if (cur != user and cur_p.privilege <= user_p.privilege)
    throw Error("access denied");
  return user_p;
}

user_profile Account::modify_profile(username_t cur, username_t user,
                                     std::string gp, std::string gn,
                                     std::string gm, std::string gg) {
  if (!logged_in(cur)) throw Error("user not logged in");
  if (!exists(user)) throw Error("user does not exist");
  user_profile cur_p = db.get(cur), user_p = db.get(user);
  if (cur != user and cur_p.privilege <= user_p.privilege)
    throw Error("access denied");

  if (gp != "") user_p.password = password_converter(gp);
  if (gn != "") user_p.realname = realname_converter(gn);
  if (gm != "") user_p.mail = mail_converter(gm);
  if (gg != "") {
    if (string2non_negative(gg) < cur_p.privilege) {
      if (gg != "") user_p.privilege = string2non_negative(gg);
    } else
      throw Error("access denied");
  }
  db.modify(user, user_p);
  return user_p;
}

invoice_t Account::get_invoice(username_t user, int order_id) {
  user_profile up = db.get(user);
  return vec_ass.get<invoice_t>(up.invoices, order_id);
}

void Account::put_invoice(invoice_t inv) {
  user_profile up = db.get(inv.username);
  vec_ass.update<invoice_t>(up.invoices, inv.invoice_id, inv);
  db.modify(inv.username, up);
}
