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
   (define string-checker-table (make-hash-table))
   (define string-converter-table (make-hash-table))

   (define (register-converter/p name value)
     (put-hash-table! string-converter-table name value))

   (define-syntax register-checker
     (syntax-rules ()
       [(_ symb name)
        (put-hash-table! string-checker-table 'symb name)]
       [(_ symb name #t)
        (begin
          (put-hash-table! string-checker-table 'symb (symbol->string 'symb))
          (let ((entry (str+ (symbol->string 'symb) "_checker")))
            (register-checker symb entry)
            (apply str+ `("const auto " ,entry " = " ,name ";\n"))))]))

   (define (get-checker/p name)
     (get-hash-table string-checker-table name '*))

   (define-syntax get-checker
     (syntax-rules ()
       [(_ symb) (get-hash-table string-checker-table 'symb '*)]))

   ;; credit Chez Scheme
   (watermark))

#include <cstdio>
#include <string>
#include <iostream>

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

@(register-checker mail
  (λ ("std::string s")
    (return
      (with-checker* "s"
        (<= len "30")
        (char-range A-Z a-z 0-9 "@."))))
 #t)


// #define DF(n, len) using n##_t = cstr<len>;
@(define-syntax DF
   (syntax-rules ()
     [(_ n len)
      (begin
        (define name (symbol->string 'n))
        (printf "name = ~s\n" name)
        (define type (str+ (symbol->string 'n) "_t"))
        (define converter-name (str+ name "_converter"))
        (register-converter/p name converter-name)
        (join "\n"
          (list
            (str+ "using " type " = cstr<" (->string len) ">;")
            (str+ type " " converter-name "(std::string s) {\n"
                  "assert(" (get-checker/p 'n) "(s));\n"
                  "return string2cstr<" (->string len)  ">(s);\n"
                  "}"
                  ))
          ))]))

@(DF username 20)
@(DF password 30)
// (DF realname (1+ (* 4 5)))
// (DF mail 30)
// (DF privilege 30)

struct user_profile {
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
