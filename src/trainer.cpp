#include "trainer.h"

Trainer::Trainer(): bf("trainer.db"), db(bf) {
}

bool Trainer::add_train(train_t train) {
  if (db.exist(train.trainID))
    return false;
  db.insert(train.trainID, train);
  return true;
}

bool Trainer::delete_train(trainID_t ID) {
  if (!db.exist(ID))
    return false;
  train_t train = db.get(ID);
  if (train.released)
    return false;
  db.erase(ID);
  return true;
}

bool Trainer::release_train(trainID_t ID) {
  if (!db.exist(ID))
    return false;
  train_t train = db.get(ID);
  if (train.released)
    return false;
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

// TODO: optimize with database query
sjtu::vector<Trainer::qry_ticket_t> Trainer::query_ticket(date_t date, stationName_t start, stationName_t end,
  bool sort_by_cost) {
  sjtu::vector<Trainer::qry_ticket_t> ret;
  auto asker = [&](const train_t &train) {
    if (date < train.saleDate[0])
      return;
    int ps = -1, pt = -1;
    for (int i = 0; i < train.stationNum; ++i) {
      if (train.stationNames[i] == start)
        ps = i;
      if (train.stationNames[i] == end)
        pt = i;
    }
    if (ps == -1 or pt == -1 or ps > pt)
      return;

    time_and_date_t leaving_time = {date, train.startTime};
    for (int i = 0; i < ps; ++i) {
      leaving_time = time_and_date_advance(leaving_time, train.travelTimes[i]);
      leaving_time = time_and_date_advance(leaving_time, train.stopoverTimes[i]);
    }
    time_and_date_t arriving_time = leaving_time;
    for (int i = ps; i < pt; ++i) {
      arriving_time = time_and_date_advance(arriving_time, train.travelTimes[i]);
      if (i + 1 < pt)
        arriving_time = time_and_date_advance(arriving_time, train.stopoverTimes[i]);
    }
    int delta_date = leaving_time.first - date;
    if (!date_range(date - delta_date, train.saleDate[0], train.saleDate[1]))
      return;
    leaving_time.first -= delta_date;
    arriving_time.first -= delta_date;

    assert(ps != pt);
    int price = 0, seat = 0;
    for (int i = ps; i < pt; ++i) {
      price += train.prices[i];
      seat = std::max(seat, train.seat[date][i]);
    }
    ret.push_back((Trainer::qry_ticket_t){train.trainID, leaving_time, arriving_time, price, train.seatNum - seat});  // TODO: fill the numbers
  };
  db.forEach(asker);
  if (ret.empty())
    return ret;
  if (sort_by_cost)
    sjtu::sort(&ret[0], &ret[0] + ret.size(), [&](const auto &u, const auto &v) { return u.price < v.price; });
  else
    sjtu::sort(&ret[0], &ret[0] + ret.size(), [&](const auto &u, const auto &v) { return time_and_date_diff(u.leaving_time, u.arriving_time) < time_and_date_diff(v.leaving_time, v.arriving_time); });
  return ret;
}
