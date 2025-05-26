@(begin
   (import (macros))

   (define handlers '())

   (define (get-handler x) (str+ x "_handler"))

   (define register-handler
     (case-lambda
       [(name) (register-handler name "" "")]
       [(name must) (register-handler name must "")]
       [(name must optional)
        (set! name (str+ (->string name) ""))
        (set! handlers (cons (cons name (list must optional)) handlers))
        (let* ((args (join ", " (map (lambda (x) (str+ "[[maybe_unused]] const std::string &g" (string x))) (string->list (str+ must optional))))))
          (str+ "std::string " (get-handler name) "(" args ")"))
        ]))
   (define-syntax register-handler/s
     (syntax-rules ()
       [(_ arg ...)
        (register-handler (symbol->string 'arg) ...)]))
   (define-syntax check
     (syntax-rules ()
       [(_ fail-value (arg checker) ...)
        (str+ "if (not (" (and (str+ (get-checker checker) "(g" (->string 'arg) ")") ...) "))\n  return " fail-value ";")]))
   (define-syntax check-optional
     (syntax-rules ()
       [(_ fail-value (arg checker) ...)
        (str+ "if (not (" (and (or (== (str+ "g" (->string 'arg)) "\"\"") (str+ (get-checker checker) "(g" (->string 'arg) ")")) ...) "))\n  return " fail-value ";")]))
   (watermark* "For dispatching commands"))

#ifndef CMD_H
#define CMD_H

#include <iostream>
#include <string>
#include <cassert>

#include "typedecl.h"
#include "vector.h"
#include "account.h"
#include "trainer.h"
// #include <vector>
// namespace sjtu {
//   template <typename T> using vector = std::vector<T>;
// };

@(register-handler/s add_user cupnmg) {
  // TODO: no check -c and -g for the first user
  @(check "\"-1\""
     [c username]
     [u username]
     [p password]
     [n realname]
     [m mail]
     [g privilege])
  return accounter.add_user(@(convert-to username gc), user_profile(gu, gp, gn, gm, gg)) ? "0" : "-1";
}

@(register-handler/s login up) {
  @(check "\"-1\""
     [u username]
     [p password])
  return accounter.login(@(convert-to username gu), @(convert-to password gp)) ? "0" : "-1";
}

@(register-handler/s logout u) {
  @(check "\"-1\"" [u username])
  return accounter.logout(@(convert-to username gu)) ? "0" : "-1";
}

@(register-handler/s query_profile cu) {
  @(check "\"-1\""
     [c username]
     [u username])
  try {
    user_profile prof = accounter.query_profile(@(convert-to username gc), @(convert-to username gu));
    if (gc == "Savage" and gu == "Catapult")
      std::cerr << "! " << (std::string)accounter.db.get(@(convert-to username gc)) << '\n' << (std::string)accounter.db.get(@(convert-to username gu)) << std::endl;
    return (std::string)prof;
  } catch (const Error &e) {
    return "-1";
  }
  return assert(false), "-1";
}

@(register-handler/s modify_profile cu pnmg) {
  @(check "\"-1\""
     [c username]
     [u username])
  @(check-optional "\"-1\""
     [p password]
     [n realname]
     [m mail]
     [g privilege])
  try {
    user_profile prof = accounter.modify_profile(@(convert-to username gc), @(convert-to username gu), gp, gn, gm, gg);
    return (std::string)prof.username + " " + (std::string)prof.realname + " " + (std::string)prof.mail + " " + number2string(prof.privilege);
  } catch (const Error &) {
    return "-1";
  }
  return assert(false), "-1";
}

@(register-handler/s add_train inmspxtody) {
  train_t train(gi, gn, gs, gm, gp, gx, gt, go, gd, gy);
  return trainer.add_train(train) ? "0" : "-1";
}

@(register-handler/s delete_train i) {
  return trainer.delete_train(@(get-converter/p 'trainID)(gi)) ? "0" : "-1";
}

@(register-handler/s release_train i) {
  return trainer.release_train(@(get-converter/p 'trainID)(gi)) ? "0" : "-1";
}

@(register-handler/s query_train id) {
  try {
    train_t train = trainer.query_train(@(get-converter/p 'trainID)(gi));
    std::string res;
    res += std::string(train.trainID) + " " + train.train_type + "\n";
    price_t price = 0;
    date_t date = @(get-converter/p 'date)(gd);
    for (int i = 0; i < train.stationNum; ++i) {
      if (i)
        res += "\n";
      price += train.prices[i];
      // TODO: deal with date/time
      res += std::string(train.stationNames[i]) + " " + std::string(i == 0 ? "x" : "x" /* arriving time */) + " -> " + std::string(i + 1 == train.stationNum ? "x" : "x" /* leaving time */) + " " + number2string(price) + " " + ((i + 1 == train.stationNum) ? number2string(train.seat[date][i]) : "x");
      return res;
    }
  } catch (const Error &) {
    return "-1";
  }
  return assert(false), "42";
}

@(register-handler/s query_ticket std p) {
  assert(gp == "" or gp == "time" or gp == "cost");
  sjtu::vector<Trainer::qry_ticket_t> vec = trainer.query_ticket(@(convert-to/p 'date 'gd), @(convert-to/p 'stationName 'gs), @(convert-to/p 'stationName 'gt), gp == "cost");
  std::string ret;
  ret += number2string(vec.size()) + '\n';
  for (int i = 0; i < (int)vec.size(); ++i) {
    if (i)
      ret += '\n';
    ret += std::string(vec[i].trainID) + " " + gs + " " + time_and_date_printer(vec[i].leaving_time) + " -> " + gt + " " + time_and_date_printer(vec[i].arriving_time) + " " + number2string(vec[i].price) + " " + number2string(vec[i].seat);
  }
  return ret;
}

@(register-handler/s query_transfer std p) {
  return "sorry, not implemented!";
}

@(register-handler/s buy_ticket uidnft q) {
  auto ret = accounter.buy_ticket(@(convert-to/p 'username 'gu),
                                  @(convert-to/p 'trainID 'gi),
                                  @(convert-to/p 'date 'gd),
                                  @(convert-to/p 'stationName 'gf),
                                  @(convert-to/p 'stationName 'gt),
                                  @(convert-to/p 'seatNum 'gn),
                                  gq == "true");
  return ret.first == 1 ? number2string(ret.second) : ret.first == 0 ? "queue" : "-1";
}

@(register-handler/s query_order u) {
  return "sorry, not implemented!";
}

@(register-handler/s refund_ticket u n) {
  return "sorry, not implemented!";
}

@(register-handler/s clean) {
  return "sorry, not implemented!";
}

@(register-handler/s exit) {
  return "bye";
}

std::string dispatch(std::string line) {
  @(define eatspace "while (p < (int)line.size() and line[p] == ' ') ++p;")
  @(define endofstr "(p >= (int)line.size())")
  @(define getword (str+ "({ std::string tmp; " eatspace "while(!" endofstr " and line[p] != ' ') { tmp += line[p++]; } tmp;" "})"))

  long long time = 0;

#if WEAK_TIME
  int p = 0;
  if (line[0] == '[') {
    assert(line.size() and line[0] == '[');
    p = 1;
    for (; p < (int)line.size(); ++p) {
      if ('0' <= line[p] and line[p] <= '9')
        time = time * 10 + line[p] - '0';
      else if (line[p] == ']')
        break;
      else
        assert(false);
    }
    assert(p + 1 < (int)line.size() and line[p] == ']' and line[p + 1] == ' ');
    ++p;
  } else
    time = -1;
#else
  assert(line.size() and line[0] == '[');
  int p = 1;
  for (; p < (int)line.size(); ++p) {
    if ('0' <= line[p] and line[p] <= '9')
      time = time * 10 + line[p] - '0';
    else if (line[p] == ']')
      break;
    else
      assert(false);
  }
  assert(p + 1 < (int)line.size() and line[p] == ']' and line[p + 1] == ' ');
  ++p;
#endif

  std::string time_str = "[" + number2string(time) + "] ";

  @eatspace
  std::string cmd = @getword;

  sjtu::vector<std::pair<std::string, std::string>> kvpair;
  while (p < (int)line.size()) {
    @eatspace
    if (@endofstr)
      break;
    assert(p + 2 < (int)line.size() and line[p] == '-' and line[p + 2] == ' ');
    std::string k(1, line[p + 1]), v = "";
    p += 2;
    @eatspace
    while (!@endofstr and line[p] != ' ')
      v += line[p++];
    kvpair.push_back({k, v});
  }

  // for (auto [k, v] : kvpair)
  //   printf("%s => %s\n", k.data(), v.data());

  // sjtu::vector<std::string> values;
  // for (auto &[k, v] : kvpair)
  //   if (k == "a")
  //     values.push_back(v);
  // assert(values.size() <= 1);
  // arguments.push_back(values[0]);

  @(define (caller cmd)
     (define must-optional-pair (cdr (assoc cmd handlers)))
     (define must (car must-optional-pair))
     (define optional (cadr must-optional-pair))
     (define all (str+ must optional))

     (define base-stmt "sjtu::vector<std::string> arguments;")
     (define call-stmt
       (let ()
         (define numbers
           (let loop ((n 0) (m (string-length all)) (r '()))
             (cond
               [(equal? m 0) r]
               [else (loop (1+ n) (1- m) (append r (list n)))])))
         (define arguments
           (map (lambda (n) (str+ "arguments[" (number->string n) "]")) numbers))
         (str+/flatten "return time_str + " (get-handler cmd) "(" (join ", " arguments) ");")))

     (define chk-must-stmt
       (map
         (lambda (now)
           (str+
             "{sjtu::vector<std::string> values;\
             for (auto &[k, v] : kvpair)\
             if (k == \"" (string now) "\")\
             values.push_back(v);\
             assert(values.size() == 1);\
             arguments.push_back(values[0]);}"))
         (string->list must)))
     (define chk-optional-stmt
       (map
         (lambda (now)
           (str+
             "{sjtu::vector<std::string> values;\
             for (auto &[k, v] : kvpair)\
             if (k == \"" (string now) "\")\
             values.push_back(v);\
             assert(values.size() <= 1);\
             if (!values.empty()) arguments.push_back(values[0]);\
             else arguments.push_back(\"\"); }"))
         (string->list optional)))

     (str+/flatten
       base-stmt
       chk-must-stmt
       chk-optional-stmt
       call-stmt))

  @(define form
     (map
       (lambda (box)
         (let ((name (car box)))
           `((== "cmd" ,(string-literal name))
             (caller ,name))))
       handlers))
  @(eval (cons 'COND form))
  
  return time_str + "unrecognized command!";
}
#endif
