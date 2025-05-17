(load "macro-util.ss")

(library (csm)
  (export and or <= > ==)
  (import (except (chezscheme) and or guard <= >)
          (csm util))
  (export (import (csm util)))
  (export (import (except (scheme) and or guard <= >)))

  (define (and a . r)
    (apply string-append `("(" ,(! a) ,@(flatten (map (lambda (x) (list " && " (! x))) r)) ")")))

  (define (or a . r)
    (apply string-append `("(" ,(! a) ,@(flatten (map (lambda (x) (list " || " (! x))) r)) ")")))

  (define (<= a b)
    (! (string-append (! a) "<=" (! b))))

  (define > (op ">"))
  (define == (op "=="))
  )

#;(display
  (and "1 < 2"
       "flag == true"
       (or "1 + 1 == 2"
           "2")))
