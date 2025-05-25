@(begin
   (import (macros))

   ;; characters
   (define a-z "abcdefghijklmnopqrstuvwxyz")
   (define A-Z "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
   (define 0-9 "0123456789")
   (define bar-ascii "|")
   (define other-ascii "!\"#$%&´()*+,-./:;<=>?@[\\]^_`{}~")
   (define visible-ascii (str+ a-z A-Z 0-9 bar-ascii other-ascii))

   ;; register-checker table
   (define string-checker-table (make-hashtable string-hash equal?))
   (define string-converter-table (make-hashtable string-hash equal?))

   (define (print-checkers)
     (let-values ([(ks vs) (hashtable-entries string-checker-table)])
       (printf "hash:\n~s\n~s\n" ks vs)))

   (define (register-converter/p name value)
     (->string! name value)
     (when (not (string? name)) (error 'converter "not a string"))
     (hashtable-set! string-converter-table name value))

   (define (register-checker/p name value)
     (->string! name value)
     (when (not (string? name)) (error 'converter "not a string"))
     (hashtable-set! string-checker-table name value))

   (define-syntax register-checker
     (syntax-rules ()
       [(_ symb name)
        (hashtable-set! string-checker-table (->string 'symb) name)]
       [(_ symb name #t)
        (begin
          (let ((entry (str+ (symbol->string 'symb) "_checker")))
            (register-checker symb entry)
            (apply str+ `("const auto " ,entry " = " ,name ";\n"))))]))

   (define (get-checker/p name)
     (when (not (string? name)) (error 'get-checker (format "~s not a string" name)))
     (hashtable-ref string-checker-table name '*))

   (define (get-converter/p name)
     (->string! name)
     (when (not (string? name)) (error 'get-checker (format "~s not a string" name)))
     (hashtable-ref string-converter-table name '*))

   (define-syntax get-checker
     (syntax-rules ()
       [(_ symb) (get-checker/p (symbol->string 'symb))]))

   (define-syntax get-converter
     (syntax-rules ()
       [(_ symb) (get-converter/p (symbol->string 'symb))]))

   (define (convert-to/p name val)
     (->string! name val)
     (format "~a(~a)" (get-converter/p name) val))

   (define-syntax convert-to
     (syntax-rules ()
       [(_ type name)
        (symbol? (syntax->datum #'name))
        (str+ (get-converter type) "(" (->string 'name) ")")]
       [(_ type name)
        (str+ (get-converter type) "(" name ")")]))

   ;; credit Chez Scheme
   (watermark))

#ifndef TYPEDECL_H
#define TYPEDECL_H

#include <cstdio>
#include <string>
#include <iostream>
#include <cassert>

#include "db/util.h"

@(register-checker username
   (λ ("std::string s")
     (return
       (with-checker* "s"
         (<= len "20")
         ((op ">") len "0")
         (char-range A-Z a-z 0-9 "_")
         (char-in "s[0]" (string-literal (str+ A-Z a-z))))))
#t)

@(register-checker password
   (λ ("std::string s")
     (return
       (with-checker* "s"
         (<= len "30")
         (> len "0")
         (char-range visible-ascii))))
  #t)

@(register-checker realname "realname_checker")
static bool realname_checker(std::string s) {
  // TODO: check UTF8 hans character
  return 2 <= s.length() and s.length() <= 20 and @(utf8-hans-len-range "s" 2 5);
}

@(register-checker mail
  (λ ("std::string s")
    (return
      (with-checker* "s"
        (<= len "30")
        (char-range A-Z a-z 0-9 "@."))))
 #t)

@(register-checker privilege "privilege_checker")
static bool privilege_checker(std::string s) {
  int x = string2non_negative(s);
  return 0 <= x and x <= 10;
}

@(register-checker trainID
   (λ ("std::string s")
     (return
       (with-checker* "s"
         (<= len "20")
         (char-range A-Z a-z 0-9 "_")
         (char-in "s[0]" (string-literal (str+ A-Z a-z))))))
   #t)

@(register-checker seatNum
   (λ ("std::string s")
     (return
       (with-checker* "s"
         (<= len "6")
         (char-range 0-9)
         (num-in "string2non_negative(s)" 0 100000))))
   #t)

@(register-checker stationName
   (λ ("std::string s")
     (return
       (with-checker* "s"
         (<= len "40")
         (utf8-hans-len-range "s" 0 10))))
   #t)

// #define DF(n, len) using n##_t = cstr<len>;
@(define-syntax DF
   (syntax-rules ()
     [(_ n len)
      (begin
        (define name (symbol->string 'n))
        (define type (str+ (symbol->string 'n) "_t"))
        (define converter-name (str+ name "_converter"))
        (register-converter/p name converter-name)
        (join "\n"
          (list
            ($f "using ~a = cstr<~a>;\n" type len)
            (str+ ($f "static ~a ~a(std::string s) {\n" type converter-name)
                  ($f "assert(~a(s));\n" (get-checker/p (->string 'n)))
                  ($f "return string2cstr<~a>(s);\n" len)
                  "}"))
          ))]))

@(DF username 20)
@(DF password 30)
@(DF realname (* 4 5))
@(DF mail 30)
@(DF trainID 20)
@(DF stationName 40)

@(register-converter/p "privilege" "string2non_negative")
@(register-converter/p "stationNum" "string2non_negative")
@(register-converter/p "price" "string2non_negative")
@(register-converter/p "duration" "string2non_negative")
@(register-converter/p "seatNum" "string2non_negative")
@(register-checker/p "price" (λ ("std::string s")
      (return (and (with-checker "s" (char-range 0-9))
                (format "string2non_negative(s) <= 100000")))))
@(register-checker/p "duration" (λ ("std::string s")
      (return (and (with-checker "s" (char-range 0-9))
                (format "string2non_negative(s) <= 10000")))))
@(register-checker/p "seatNum" (λ ("std::string s")
      (return (and (with-checker "s" (char-range 0-9))
                (format "string2non_negative(s) <= 100000")))))
@(register-checker/p "stationNum" (λ ("std::string s")
      (return (and (with-checker "s" (char-range 0-9))
                (format "string2non_negative(s) <= 100")
                (format "string2non_negative(s) >= 2")))))
using privilege_t = int;
using stationNum_t = int;
using Time_t = int;
using price_t = int;
using duration_t = int;
using seatNum_t = int;
using date_t = __int8_t;
using train_type_t = char;
using time_and_date_t = std::pair<date_t, Time_t>;

static std::string date_printer(date_t d) {
  int mm = d < 30 ? 6 : d < 61 ? 7 : d < 92 ? 8 : -1;
  int dd = 1 + (d < 30 ? d : d < 61 ? d - 30 : d < 92 ? d - 61 : -1);
  static char buf[20];
  sprintf(buf, "%2-%2", mm, dd);
  return std::string(buf);
}

static std::string time_printer(Time_t d) {
  int hh = d / 60;
  int mm = d % 60;
  static char buf[20];
  sprintf(buf, "%2:%2", hh, mm);
  return std::string(buf);
}

static std::string time_and_date_printer(const time_and_date_t &x) {
  return date_printer(x.first) + " " + time_printer(x.second);
}

inline static int time_and_date_diff(const time_and_date_t &s, const time_and_date_t &t) {
  return (t.first - s.first) * 1440 + (t.second - s.second);
}

static time_and_date_t time_and_date_advance(time_and_date_t t, duration_t delta) {
  int min = t.second + delta;
  t.second = min % 1440;
  t.first += min / 1440;
  return t;
}

@(register-checker/p 'train_type
   (λ ("std::string s")
     (return
       (with-checker "s"
         (== len "1")
         (char-range ($lit A-Z))))))
@(register-converter/p 'train_type
    (λ ("std::string s")
      (return "s[0]")))

const Time_t invalid_time = -1;
static Time_t Time_converter(std::string s) {
  if (not(s.size() == 5 and s[2] == ':'))
    return invalid_time;
  for (int i = 0; i < 5; ++i)
    if (i != 2 and not @(char-in "s[i]" ($lit 0-9)))
      return invalid_time;
  return (s[0] - '0') * 600 + (s[1] - '0') * 60 + (s[3] - '0') * 10 + (s[4] - '0');
}
@(register-converter/p "Time" "Time_converter")
const auto Time_checker = @(λ ("std::string s") "return (Time_converter(s) != invalid_time);");
@(register-checker/p "Time" "Time_checker")

const date_t invalid_date = -1;  // 0xFFFF
static date_t date_converter(std::string s) {
  if (not(s.size() == 5 and s[2] == '-'))
    return invalid_date;
  for (int i = 0; i < 5; ++i)
    if (i != 2 and not @(char-in "s[i]" ($lit 0-9)))
      return invalid_date;
  date_t mm = (s[0] - '0') * 10 + s[1] - '0';
  date_t dd = (s[3] - '0') * 10 + s[4] - '0';
  if (not(@or("mm == 6 and dd <= 30"
              "mm == 7 and dd <= 31"
              "mm == 8 and dd <= 31")))
    return invalid_date;
  return dd - 1 + (mm >= 7) * 30 + (mm >= 8) * 31;
}
const auto date_checker = @(λ ("std::string s") "return date_converter(s) != invalid_date;");
@(register-converter/p 'date 'date_converter)
@(register-checker/p 'date 'date_checker)

@(define (init/p field type var)
   (->string! field type var)
   (str+
    "assert(" (get-checker/p type) "(" var "));\n"
    field " = " (get-converter/p type) "(" var ");"))

struct user_profile {
  username_t username;
  password_t password;
  realname_t realname;
  mail_t mail;
  privilege_t privilege;
  user_profile() = default;
  user_profile(std::string s0, std::string s1, std::string s2, std::string s3, std::string s4) {
    @(init/p 'username 'username 's0)
    @(init/p 'password 'password 's1)
    @(init/p 'realname 'realname 's2)
    @(init/p 'mail 'mail 's3)
    @(init/p 'privilege 'privilege 's4)
  }
  explicit operator std::string() {
    return (std::string)username + " " + (std::string)realname + " " + (std::string)mail + " " + number2string(privilege);
  }
};

struct train_t {
  // original field
  trainID_t trainID;
  stationNum_t stationNum;
  sjtu::array<stationName_t, 100> stationNames;
  seatNum_t seatNum;
  sjtu::array<price_t, 99> prices;
  Time_t startTime;
  sjtu::array<duration_t, 99> travelTimes;
  sjtu::array<duration_t, 98> stopoverTimes;
  date_t saleDate[2];
  train_type_t train_type;

  // additional field
  bool released;
  sjtu::array<sjtu::array<int, 99>, 92> seat;
  train_t() = default;

  @(define (init/arr-p field type var bound)
     (define tmp (genname "tmpvar"))
     (->string! field type var)
     (str+
       (format "auto ~a = split(~a);\n" tmp var)
       ; (format "assert(~a.size() <= ~a);\n" tmp bound)
       (format "Eassert(~a.size() == (~a));\n" tmp bound)
       (format "for (size_t i = 0; i < ~a.size(); ++i) {\n" tmp)
       (format "  ~a[i] = (~a(~a[i]));\n" field (get-converter/p type) tmp)
       "}"))
  train_t(@(join ", " (map (lambda (x) (format "std::string s~a" x)) (list/range 0 10)))) {
    @(init/p 'trainID 'trainID 's0)
    @(init/p 'stationNum 'stationNum 's1)
    @(init/arr-p 'stationNames 'stationName 's2 "stationNum")
    @(init/p 'seatNum 'seatNum 's3)
    @(init/arr-p 'prices 'price 's4 "stationNum - 1")
    @(init/p 'startTime 'Time 's5)
    @(init/arr-p 'travelTimes 'duration 's6 "stationNum - 1")
    @(init/arr-p 'stopoverTimes 'duration 's7 "stationNum - 2")
    @(init/arr-p 'saleDate 'date 's8 2)
    @(init/p 'train_type 'train_type 's9)

    released = false;
  }
};

inline static bool date_range(date_t x, date_t s, date_t t) { return x <= s and s <= t; }

/*
int main() {
  std::string s;
  std::cin >> s;
  std::cout << @(get-checker username)(s) << std::endl;
  std::cout << @(get-checker password)(s) << std::endl;
  std::cout << @(get-checker mail)(s) << std::endl;
  return 0;
}
*/

#endif
