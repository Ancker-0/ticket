#include "trainer.h"

Trainer::Trainer(): bf("trainer.db"), vec_ass(bf), db(bf) {
}

bool Trainer::add_train(const train_t &train) {
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

void Trainer::update_train(const train_t &train) {
  Eassert(db.exist(train.trainID), "train does not exist");
  db.modify(train.trainID, train);
  errf("pending queue size of %s = %lu\n", train.trainID.data(), vec_ass.getAll<invoice_t>(train.queue).size());
}

void Trainer::pend(const invoice_t &invoice) {
  assert(db.exist(invoice.trainID));
  train_t train = db.get(invoice.trainID);
  assert(train.released);
  vec_ass.push_back(train.queue, invoice);
  update_train(train);
}

void Trainer::refund_ticket(trainID_t ID, date_t date, stationName_t from, stationName_t to, seatNum_t num) {
  train_t train = query_train(ID);
  int ps = train.get_station_id(from), pt = train.get_station_id(to);
  for (int i = ps; i < pt; ++i) {
    assert(train.seat[date][i] >= num);
    train.seat[date][i] -= num;
  }

  sjtu::vector<invoice_t> blah = vec_ass.getAll<invoice_t>(train.queue);
  errf("blah size of %s = %lu\n", train.trainID.data(), blah.size());
  for (int i = 0; i < blah.size(); ++i) {
    blah[i] = accounter.get_invoice(blah[i].username, blah[i].invoice_id);
    if (blah[i].status == 0) {
      ps = train.get_station_id(blah[i].from), pt = train.get_station_id(blah[i].to);
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
        errf("Pending success! %s for train %s\n", blah[i].username, blah[i].trainID);
      }
      errf("Pending fail! %s for train %s\n", blah[i].username, blah[i].trainID);
    }
  }
  vec_ass.putAll(train.queue, blah);
  update_train(train);
}

// TODO: optimize with database query
sjtu::vector<Trainer::qry_ticket_t> Trainer::query_ticket(date_t date, stationName_t start, stationName_t end,
  bool sort_by_cost) {
  sjtu::vector<Trainer::qry_ticket_t> ret;
  auto asker = [&](const train_t &train) {
    if (date < train.saleDate[0])
      return;
    int ps = train.get_station_id(start), pt = train.get_station_id(end);
    if (ps == -1 or pt == -1 or ps > pt)
      return;

    // TODO: refactor using "typedecl.h"/arrive_time()
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
    date -= delta_date;

    assert(ps != pt);
    int price = 0, seat = 0;
    for (int i = ps; i < pt; ++i) {
      price += train.prices[i];
      seat = std::max(seat, train.seat[date][i]);
    }
    assert(train.seatNum >= seat);
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
