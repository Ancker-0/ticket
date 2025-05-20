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
     (when (not (string? name)) (error 'get-checker (format "~s not a string" name)))
     (hashtable-ref string-converter-table name '*))

   (define-syntax get-checker
     (syntax-rules ()
       [(_ symb) (get-checker/p (symbol->string 'symb))]))

   (define-syntax get-converter
     (syntax-rules ()
       [(_ symb) (get-converter/p (symbol->string 'symb))]))

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
  return 2 <= s.length() and s.length() <= 20;
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
            (str+ "using " type " = cstr<" (->string len) ">;")
            (str+ "static " type " " converter-name "(std::string s) {\n"
                  "assert(" (get-checker/p (->string 'n)) "(s));\n"
                  "return string2cstr<" (->string len)  ">(s);\n"
                  "}"
                  ))
          ))]))

@(DF username 20)
@(DF password 30)
@(DF realname (* 4 5))
@(DF mail 30)
@(DF trainID 20)
@(register-converter/p "privilege" "string2non_negative")
using privilege_t = int;
@(register-converter/p "stationNum" "string2non_negative")
using stationNum = int;

struct user_profile {
  username_t username;
  password_t password;
  realname_t realname;
  mail_t mail;
  privilege_t privilege;
  user_profile() = default;
  @(define (init/p field type var)
     (->string! field type var)
     (str+
      "assert(" (get-checker/p type) "(" var "));\n"
      field " = " (get-converter/p type) "(" var ");"))
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
