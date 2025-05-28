#include "trainer.h"

Trainer::Trainer()
    : bf("trainer.db"), bf2("trainer-vec.db"), vec_ass(bf2), db(bf) {}

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
  transer.register_train(train);
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
      for (int j = ps; j < pt; ++j)
        if (train.seat[blah[i].realdate][j] + blah[i].num > train.seatNum) {
          success = false;
          break;
        }
      if (success) {
        for (int j = ps; j < pt; ++j)
          train.seat[blah[i].realdate][j] += blah[i].num;
        blah[i].status = 1;
        accounter.put_invoice(blah[i]);
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
  sjtu::vector<Trans::transfer_t> vec = transer.ask(start, end);
  sjtu::vector<Trainer::qry_ticket_t> ret;
  for (int i = 0; i < (int)vec.size(); ++i) {
    date_t realdate = date - vec[i].leav_time.first;
    if (after == invalid_time) {
      if (!date_range(realdate, vec[i].saleDate[0], vec[i].saleDate[1]))
        continue;
    } else {
      if (realdate > vec[i].saleDate[1]) continue;
      realdate = std::max(realdate, vec[i].saleDate[0]);
      if (time_and_date_less(
              {realdate + vec[i].leav_time.first, vec[i].leav_time.second},
              {date, after}))
        ++realdate;
      if (realdate > vec[i].saleDate[1]) continue;
    }

    train_t train = db.get(vec[i].trainID);
    if (train.trainID != vec[i].trainID or
        train.stationNames[vec[i].s] != start or
        train.stationNames[vec[i].t] != end)
      continue;
    int s = vec[i].s, t = vec[i].t;
    int price = 0;
    seatNum_t seat = 0;
    for (int j = s; j < t; ++j) {
      price += train.prices[j];
      seat = std::max(seat, train.seat[realdate][j]);
    }

    vec[i].leav_time.first += realdate;
    vec[i].arr_time.first += realdate;
    ret.push_back(qry_ticket_t{vec[i].trainID, vec[i].leav_time,
                               vec[i].arr_time, price, train.seatNum - seat});
  }

  /*
  return sjtu::vector<Trainer::qry_ticket_t>{};  // TODO: for test ONLY!
  auto asker = [&](const train_t &train) {
    if (!train.released)
      return;
    if (after == invalid_time and date < train.saleDate[0])
      return;
    int ps = train.get_station_id(start), pt = train.get_station_id(end);
    if (ps == -1 or pt == -1 or ps >= pt)
      return;

    // TODO: refactor using "typedecl.h"/arrive_time()
    time_and_date_t leaving_time = train.leaving_time(start);
    time_and_date_t arriving_time = train.arrive_time(end);
    date_t realdate = date - leaving_time.first;
    if (after == invalid_time) {
      if (!date_range(realdate, train.saleDate[0], train.saleDate[1]))
        return;
    } else {
      if (realdate > train.saleDate[1])
        return;
      realdate = std::max(realdate, train.saleDate[0]);
      // TODO: what if the time equals?
      if (time_and_date_less({realdate + leaving_time.first,
  leaving_time.second}, {date, after}))
        ++realdate;
      if (realdate > train.saleDate[1])
        return;
      assert(!time_and_date_less({realdate + leaving_time.first,
  leaving_time.second}, {date, after}));
    }
    leaving_time.first += realdate;
    arriving_time.first += realdate;

    assert(ps != pt);
    int price = 0, seat = 0;
    for (int i = ps; i < pt; ++i) {
      price += train.prices[i];
      seat = std::max(seat, train.seat[realdate][i]);
    }
    assert(train.seatNum >= seat);
    ret.push_back((Trainer::qry_ticket_t){train.trainID, leaving_time,
  arriving_time, price, train.seatNum - seat});  // TODO: fill the numbers
  };
  db.forEach(asker);
  */
  if (ret.empty()) return ret;
  if (sort_by_cost)
    sjtu::sort(
        &ret[0], &ret[0] + ret.size(), [&](const auto &u, const auto &v) {
          return u.price == v.price ? u.trainID < v.trainID : u.price < v.price;
        });
  else
    sjtu::sort(&ret[0], &ret[0] + ret.size(),
               [&](const auto &u, const auto &v) {
                 if (time_and_date_diff(u.leaving_time, u.arriving_time) ==
                     time_and_date_diff(v.leaving_time, v.arriving_time))
                   return u.trainID < v.trainID;
                 return time_and_date_diff(u.leaving_time, u.arriving_time) <
                        time_and_date_diff(v.leaving_time, v.arriving_time);
               });
  return ret;
}

Trainer::transfer_t Trainer::query_transfer(date_t date, stationName_t start,
                                            stationName_t end,
                                            bool sort_by_cost) {
  // return transfer_t{};  // TODO: for test ONLY!

  sjtu::vector<transfer_t> ret;

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
      seat = std::max(seat, train.seat[realdate][i - 1]);
      time_and_date_t arriving_time = train.arrive_time(train.stationNames[i]);
      arriving_time.first += realdate;
      sjtu::vector<qry_ticket_t> vec =
          query_ticket(arriving_time.first, train.stationNames[i], end,
                       sort_by_cost, arriving_time.second);
      if (vec.empty()) continue;
      sjtu::sort(&vec[0], &vec[0] + vec.size(), [&](auto u, auto v) {
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
        ret.push_back(
            transfer_t{qry_ticket_t{train.trainID, leaving_time, arriving_time,
                                    price, train.seatNum - seat},
                       vec[0], train.stationNames[i]});
      }
    }
  };

  db.forEach(asker);
  if (ret.empty()) return transfer_t{};
  sjtu::sort(&ret[0], &ret[0] + ret.size(), [&](auto u, auto v) {
    int priceu = u.q1.price + u.q2.price;
    int timeu = time_and_date_diff(u.q1.leaving_time, u.q2.arriving_time);
    int pricev = v.q1.price + v.q2.price;
    int timev = time_and_date_diff(v.q1.leaving_time, v.q2.arriving_time);
    if (sort_by_cost) {
      if (((priceu) == (pricev))) {
        if (((timeu) == (timev))) {
          if (((u.q1.trainID) == (v.q1.trainID))) {
            return ((u.q2.trainID) < (v.q2.trainID));
          } else {
            return ((u.q1.trainID) < (v.q1.trainID));
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
          if (((u.q1.trainID) == (v.q1.trainID))) {
            return ((u.q2.trainID) < (v.q2.trainID));
          } else {
            return ((u.q1.trainID) < (v.q1.trainID));
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
