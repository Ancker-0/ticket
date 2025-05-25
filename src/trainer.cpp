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
    if (!date_range(date, train.saleDate[0], train.saleDate[1]))
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
    assert(ps != pt);
    ret.push_back({train.trainID, {0, 0}, {0, 0}, 0, 0});  // TODO: fill the numbers
  };
  db.forEach(asker);
  return ret;
}
