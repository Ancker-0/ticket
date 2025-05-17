@(begin
   (import (macros))
   (define a-z "abcdefghijklmnopqrstuvwxyz")
   (define A-Z "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
   (define 0-9 "0123456789")
   (define bar-ascii "|")
   (define other-ascii "!\"#$%&´()*+,-./:;<=>?@[\\]^_`{}~")
   (define visible-ascii (str+ a-z A-Z 0-9 bar-ascii other-ascii))
   (watermark))
#include <cstdio>
#include <iostream>
#include <string>

bool valid_username(std::string s) {
  @(with-checker "s"
     (guard "false"
       (and (<= len "20")
            ((op ">") len "0")
            (char-range A-Z a-z 0-9 "_")
            (char-in "s[0]" (string-literal (str+ A-Z a-z))))))
  return true;
}

bool valid_password(std::string s) {
  @(with-checker "s"
     (guard "false"
       (and (<= len "30")
            (> len "0")
            (char-range visible-ascii))))
  return true;
}

bool valid_mail(std::string s) {
  @(with-checker "s"
     (guard "false"
       (and (<= len "30")
            (char-range A-Z a-z 0-9 "@."))))
  return true;
}

bool valid_privilege(std::string s) {
  @(with-checker "s"
     (guard "false"
       (and (char-range 0-9)
            (or (== len "1")
                "s == \"10\""))))
  return true;
}

int main() {
  std::string s;
  std::cin >> s;
  std::cout << valid_username(s) << std::endl;
  std::cout << valid_password(s) << std::endl;
  std::cout << valid_mail(s) << std::endl;
  return 0;
}
