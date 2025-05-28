#ifndef TRANSFER_H
#define TRANSFER_H

#include "db/database.h"
#include "db/fs_vector.h"
#include "typedecl.h"
#include "hasher.h"

class Trans {
public:
  struct transfer_t {
    trainID_t trainID;
    stationName_t from, to;
    date_t saleDate[2];
    time_and_date_t leav_time, arr_time;
    int s, t;
  };

private:
  Bfsp bf;
  Database<Hasher<transferCode_t>, fs_vector::Head> db;
  fs_vector vec_ass;
  Trans();

public:
  void register_train(const train_t &train);
  static Trans &getInstance() {
    static Trans instance;
    return instance;
  }

  sjtu::vector<transfer_t> ask(stationName_t from, stationName_t to);
};

static Trans &transer = Trans::getInstance();

Trans::transfer_t extract(const train_t &train, int s, int t);

#endif
