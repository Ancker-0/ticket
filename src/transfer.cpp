#include <cstring>
#include "transfer.h"
#include "db/util.h"
#include "db/fs_vector.h"

Trans::Trans() : bf("trans.db"), db(bf), vec_ass(bf) {}

transferCode_t concat(stationName_t u, stationName_t v) {
  transferCode_t key;
  strcpy(key.data(), u.data());
  strcpy(key.data() + strlen(key.data()), v.data());
  return key;
}

Trans::transfer_t extract(const train_t &train, int s, int t) {
  return Trans::transfer_t {
    train.trainID,
    train.stationNames[s],
    train.stationNames[t],
    { train.saleDate[0], train.saleDate[1] },
    train.leaving_time(s),
    train.arrive_time(t),
    s, t
  };
}

void Trans::register_train(const train_t &train) {
  for (int i = 0; i + 1 < train.stationNum; ++i)
    for (int j = i + 1; j < train.stationNum; ++j) {
      transferCode_t key = concat(train.stationNames[i], train.stationNames[j]);
      if (!db.exist(key))
        db.insert(key, fs_vector::Head{});
      fs_vector::Head hd = db.get(key);
      vec_ass.push_back(hd, extract(train, i, j));
      db.modify(key, hd);
    }
}

sjtu::vector<Trans::transfer_t> Trans::ask(stationName_t from, stationName_t to) {
  transferCode_t key = concat(from, to);
  if (db.exist(key))
    return vec_ass.getAll<transfer_t>(db.get(key));
  return {};
}
