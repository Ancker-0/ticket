@(begin
   (import (macros))

   (define handlers '())

   (define (get-handler x) x)

   (define register-handler
     (case-lambda
       [(name) (register-handler name "" "")]
       [(name must) (register-handler name must "")]
       [(name must optional)
        (set! name (->string name))
        (set! handlers (cons (cons name (list must optional)) handlers))
        (let* ((args (join ", " (map (lambda (x) (str+ "[[maybe_unused]] const std::string &g" (string x))) (string->list (str+ must optional))))))
          (str+ "std::string " name "(" args ")"))
        ]))
   (define-syntax register-handler/s
     (syntax-rules ()
       [(_ arg ...)
        (register-handler (symbol->string 'arg) ...)]))
   (define-syntax check
     (syntax-rules ()
       [(_ fail-value (arg checker) ...)
        (str+ "if (not (" (and (str+ (conv checker) "(g" (->string 'arg) ")") ...) "))\n  return " fail-value ";")]))
   (watermark* "For dispatching commands"))

#ifndef CMD_H
#define CMD_H

#include <iostream>
#include <string>
#include <cassert>

#include "typedecl.h"
#include "vector.h"
// #include <vector>
// namespace sjtu {
//   template <typename T> using vector = std::vector<T>;
// };

@(register-handler/s add_user cupnmg) {
  @(check "\"-1\""
     [c username]
     [u username]
     [p password])
}

@(register-handler/s login up) {
}

@(register-handler/s logout u) {
}

@(register-handler/s query_profile cu) {
}

@(register-handler/s modify_profile cu pnmg) {
  printf("hi\n");
  return "ha";
}

@(register-handler/s add_train inmspxtody) {
}

@(register-handler/s delete_train i) {
}

@(register-handler/s release_train i) {
}

@(register-handler/s query_train id) {
}

@(register-handler/s query_ticket std p) {
}

@(register-handler/s query_transfer std p) {
}

@(register-handler/s buy_ticket uidnft q) {
}

@(register-handler/s query_order u) {
}

@(register-handler/s refund_ticket u n) {
}

@(register-handler/s clean) {
}

@(register-handler/s exit) {
}

void dispatch(std::string line) {
  @(define eatspace "while (p < (int)line.size() and line[p] == ' ') ++p;")
  @(define endofstr "(p >= (int)line.size())")
  @(define getword (str+ "({ std::string tmp; " eatspace "while(!" endofstr " and line[p] != ' ') { tmp += line[p++]; } tmp;" "})"))

  long long time = 0;
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

  for (auto [k, v] : kvpair)
    printf("%s => %s\n", k.data(), v.data());

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
         (str+/flatten (get-handler cmd) "(" (join ", " arguments) ");")))

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
}
#endif
