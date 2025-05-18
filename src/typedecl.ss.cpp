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
   (define string-converter-table (make-hash-table))

   (define-syntax register-checker
     (syntax-rules ()
       [(_ symb name)
        (put-hash-table! string-converter-table 'symb name)]
       [(_ symb name #t)
        (begin
          (put-hash-table! string-converter-table 'symb (symbol->string 'symb))
          (let ((entry (str+ (symbol->string 'symb) "_checker")))
            (register-checker symb entry)
            (apply str+ `("const auto " ,entry " = " ,name ";\n"))))]))

   (define-syntax conv
     (syntax-rules ()
       [(_ symb) (get-hash-table string-converter-table 'symb '*)]))

   ;; credit Chez Scheme
   (watermark))

#include <cstdio>
#include <string>
#include <iostream>

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

int main() {
  std::string s;
  std::cin >> s;
  std::cout << @(conv username)(s) << std::endl;
  std::cout << @(conv password)(s) << std::endl;
  std::cout << @(conv mail)(s) << std::endl;
  return 0;
}
