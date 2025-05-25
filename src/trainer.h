#ifndef TRAINER_H
#define TRAINER_H

#include "typedecl.h"
#include "db/fs.h"
#include "db/database.h"
#include "vector.h"

class Trainer {
  Bfsp bf;
  Trainer();

public:
  Database<trainID_t, train_t> db;

  bool add_train(train_t train);
  bool delete_train(trainID_t ID);
  bool release_train(trainID_t ID);
  train_t query_train(trainID_t ID);

  static Trainer &getInstance() {
    static Trainer instance;
    return instance;
  }

  struct qry_ticket_t {
    trainID_t trainID;
    time_and_date_t leaving_time, arriving_time;
    int price, seat;
  };
  sjtu::vector<qry_ticket_t> query_ticket(date_t date, stationName_t start, stationName_t end, bool sort_by_cost);
};

static Trainer &trainer = Trainer::getInstance();
#endif //TRAINER_H