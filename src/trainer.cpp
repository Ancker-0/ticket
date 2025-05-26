#include "trainer.h"

Trainer::Trainer() : bf("trainer.db"), vec_ass(bf), db(bf) {}

bool Trainer::add_train(const train_t &train) {
  if (db.exist(train.trainID)) return false;
  db.insert(train.trainID, train);
  return true;
}

bool Trainer::delete_train(trainID_t ID) {
  if (!db.exist(ID)) return false;
  train_t train = db.get(ID);
  if (train.released) return false;
  db.erase(ID);
  return true;
}

bool Trainer::release_train(trainID_t ID) {
  if (!db.exist(ID)) return false;
  train_t train = db.get(ID);
  if (train.released) return false;
  train.released = true;
  db.modify(ID, train);
  return true;
}

train_t Trainer::query_train(trainID_t ID) {
  Eassert(db.exist(ID), "train does not exist");
  train_t train = db.get(ID);
  // Eassert(train.released, "unreleased train");
  return train;
}

void Trainer::update_train(const train_t &train) {
  Eassert(db.exist(train.trainID), "train does not exist");
  db.modify(train.trainID, train);
}

void Trainer::pend(const invoice_t &invoice) {
  assert(db.exist(invoice.trainID));
  train_t train = db.get(invoice.trainID);
  assert(train.released);
  vec_ass.push_back(train.queue, invoice);
  update_train(train);
}

void Trainer::refund_ticket(trainID_t ID, date_t date, stationName_t from,
                            stationName_t to, seatNum_t num) {
  train_t train = query_train(ID);
  int ps = train.get_station_id(from), pt = train.get_station_id(to);
  for (int i = ps; i < pt; ++i) {
    assert(train.seat[date][i] >= num);
    train.seat[date][i] -= num;
  }

  sjtu::vector<invoice_t> blah = vec_ass.getAll<invoice_t>(train.queue);
  for (int i = 0; i < blah.size(); ++i) {
    blah[i] = accounter.get_invoice(blah[i].username, blah[i].invoice_id);
    if (blah[i].status == 0) {
      ps = train.get_station_id(blah[i].from),
      pt = train.get_station_id(blah[i].to);
      bool success = true;
      for (int i = ps; i < pt; ++i)
        if (train.seat[blah[i].realdate][i] + blah[i].num > train.seatNum) {
          success = false;
          break;
        }
      if (success) {
        for (int i = ps; i < pt; ++i)
          train.seat[blah[i].realdate][i] += blah[i].num;
        blah[i].status = 1;
        // errf("Pending success! %s for train %s\n", blah[i].username,
        // blah[i].trainID);
      }
      // errf("Pending fail! %s for train %s\n", blah[i].username,
      // blah[i].trainID);
    }
  }
  vec_ass.putAll(train.queue, blah);
  update_train(train);
}

// TODO: optimize with database query
sjtu::vector<Trainer::qry_ticket_t> Trainer::query_ticket(date_t date,
                                                          stationName_t start,
                                                          stationName_t end,
                                                          bool sort_by_cost,
                                                          Time_t after) {
  sjtu::vector<Trainer::qry_ticket_t> ret;
  auto asker = [&](const train_t &train) {
    if (!train.released) return;
    if (date < train.saleDate[0]) return;
    int ps = train.get_station_id(start), pt = train.get_station_id(end);
    if (ps == -1 or pt == -1 or ps > pt) return;

    // TODO: refactor using "typedecl.h"/arrive_time()
    time_and_date_t leaving_time = train.leaving_time(start);
    time_and_date_t arriving_time = train.arrive_time(end);
    date_t realdate = date - leaving_time.first;
    leaving_time.first += realdate;
    arriving_time.first += realdate;
    if (after == invalid_time) {
      if (!date_range(realdate, train.saleDate[0], train.saleDate[1])) return;
    } else {
      if (realdate > train.saleDate[1]) return;
      realdate = std::max(realdate, train.saleDate[0]);
      // TODO: what if the time equals?
      if (time_and_date_less(
              {realdate + leaving_time.first, leaving_time.second},
              {date, after}))
        ++realdate;
      if (realdate > train.saleDate[1]) return;
      assert(!time_and_date_less(
          {realdate + leaving_time.first, leaving_time.second}, {date, after}));
    }

    assert(ps != pt);
    int price = 0, seat = 0;
    for (int i = ps; i < pt; ++i) {
      price += train.prices[i];
      seat = std::max(seat, train.seat[realdate][i]);
    }
    assert(train.seatNum >= seat);
    ret.push_back((Trainer::qry_ticket_t){
        train.trainID, leaving_time, arriving_time, price,
        train.seatNum - seat});  // TODO: fill the numbers
  };
  db.forEach(asker);
  if (ret.empty()) return ret;
  if (sort_by_cost)
    sjtu::sort(&ret[0], &ret[0] + ret.size(),
               [&](const auto &u, const auto &v) { return u.price < v.price; });
  else
    sjtu::sort(&ret[0], &ret[0] + ret.size(),
               [&](const auto &u, const auto &v) {
                 return time_and_date_diff(u.leaving_time, u.arriving_time) <
                        time_and_date_diff(v.leaving_time, v.arriving_time);
               });
  return ret;
}

sjtu::vector<Trainer::qry_ticket_t> Trainer::query_transfer(date_t date,
                                                            stationName_t start,
                                                            stationName_t end,
                                                            bool sort_by_cost) {
  sjtu::vector<sjtu::vector<Trainer::qry_ticket_t>> ret;

  auto asker = [&](const train_t &train) {
    if (!train.released) return;
    int ps = train.get_station_id(start);
    if (ps == -1) return;

    time_and_date_t leaving_time = train.leaving_time(start);
    date_t realdate = date - leaving_time.first;
    leaving_time.first += realdate;
    if (!date_range(realdate, train.saleDate[0], train.saleDate[1])) return;

    int price = 0;
    seatNum_t seat = 0;
    for (int i = ps + 1; i < train.stationNum; ++i) {
      price += train.prices[i - 1];
      seat = std::min(seat, train.seat[realdate][i - 1]);
      time_and_date_t arriving_time = train.arrive_time(end);
      arriving_time.first += realdate;
      sjtu::vector<qry_ticket_t> vec =
          query_ticket(arriving_time.first, train.stationNames[i], end,
                       sort_by_cost, arriving_time.second);
      if (vec.empty()) continue;
      sjtu::sort(&vec[0], &vec[1], [&](auto u, auto v) {
        if (u.trainID == train.trainID) return false;
        if (v.trainID == train.trainID) return true;

        if (sort_by_cost) {
          if (((u.price) == (v.price))) {
            if (((u.arriving_time) == (v.arriving_time))) {
              return ((u.trainID) < (v.trainID));
            } else {
              return ((u.arriving_time) < (v.arriving_time));
            }
          } else {
            return ((u.price) < (v.price));
          };
        } else {
          if (((u.arriving_time) == (v.arriving_time))) {
            if (((u.price) == (v.price))) {
              return ((u.trainID) < (v.trainID));
            } else {
              return ((u.price) < (v.price));
            }
          } else {
            return ((u.arriving_time) < (v.arriving_time));
          };
        }
      });
      if (vec[0].trainID != train.trainID) {
        sjtu::vector<Trainer::qry_ticket_t> tmp;
        tmp.push_back(qry_ticket_t{train.trainID, leaving_time, arriving_time,
                                   price, train.seatNum - seat});
        tmp.push_back(vec[0]);
        ret.push_back(tmp);
      }
    }
  };

  db.forEach(asker);
  if (ret.empty()) return sjtu::vector<Trainer::qry_ticket_t>{};
  sjtu::sort(&ret[0], &ret[0] + ret.size(), [&](auto u, auto v) {
    int priceu = u[0].price + u[1].price;
    int timeu = time_and_date_diff(u[0].leaving_time, u[1].arriving_time);
    int pricev = v[0].price + v[1].price;
    int timev = time_and_date_diff(v[0].leaving_time, v[1].arriving_time);
    if (sort_by_cost) {
      if (((priceu) == (pricev))) {
        if (((timeu) == (timev))) {
          if (((u[0].trainID) == (v[0].trainID))) {
            return ((u[1].trainID) < (v[1].trainID));
          } else {
            return ((u[0].trainID) < (v[0].trainID));
          }
        } else {
          return ((timeu) < (timev));
        }
      } else {
        return ((priceu) < (pricev));
      }
    } else {
      if (((timeu) == (timev))) {
        if (((priceu) == (pricev))) {
          if (((u[0].trainID) == (v[0].trainID))) {
            return ((u[1].trainID) < (v[1].trainID));
          } else {
            return ((u[0].trainID) < (v[0].trainID));
          }
        } else {
          return ((priceu) < (pricev));
        }
      } else {
        return ((timeu) < (timev));
      }
    }
  });
  return ret[0];
}
