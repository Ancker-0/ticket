@(begin
   (import (macros))

   (define register-handler
     (case-lambda
       [(name) (register-handler name "" "")]
       [(name must) (register-handler name must "")]
       [(name must optional)
        (set! name (->string name))
        (let* ((args (join ", " (map (lambda (x) (str+ "const std::string &g" (string x))) (string->list (str+ must optional))))))
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

#include <string>

#include "typedecl.cpp"

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
