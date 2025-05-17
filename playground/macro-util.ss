(library (csm util)
  (export flatten begins with-checker guard !
          char-in str+ op string-literal)
  (import (except (chezscheme) guard))

(define str+ string-append)

(define (flatten l)
  (cond
    [(not (list? l)) (list l)]
    [(null? l) l]
    [else
     (let ((a (car l))
           (d (cdr l)))
       (append (flatten a) (flatten d)))]))

(define (begins . args)
  (apply string-append 
         `("({\n"
           ,@(flatten (map (lambda (x) (list x "\n")) args))
           "})")))

(define (genname)
  (let ((s (gensym->unique-string (gensym))))
    (string-append
      (substring s 0 25)
      (substring s 26 (string-length s)))))

(define (forall x y)
  (let ((name (genname))
        (flag (genname)))
    (begins
      "bool " flag " = true;"
      (string-append "for (auto &" name " : " (! x) ")\n"
                     "  if (not " (! (y name)) ") {\n"
                     "    " flag " = false;\n"
                     "    break;\n"
                     "  }")
      flag ";"
      )))

(define (forsome x y)
  (let ((name (genname))
        (flag (genname)))
    (begins
      "bool " flag " = false;"
      (string-append "for (auto &" name " : " (! x) ")\n"
                     "  if (" (y name) ") {\n"
                     "    " flag " = true;\n"
                     "    break;\n"
                     "  }")
      flag ";"
      )))

(define (op name)
  (lambda (a b)
    (! (string-append (! a) name (! b)))))

(define (char-in x y)
  (forsome y (lambda (z) ((op "==") x z))))

(define (string-literal s)
  (define (escape ch)
    (case ch
      [#\" "\\\""]
      [#\\ "\\\\"]
      [#\newline "\\n"]
      [else (string ch)]))
  (apply string-append `("\"" ,@(map escape (string->list s)) "\"")))

(define-syntax with-checker
  (lambda (x)
    (syntax-case x ()
      [(k name body ...)
       (with-syntax ([len (datum->syntax #'k 'len)]
                     [char-range (datum->syntax #'k 'char-range)])
         #'(let ((len (string-append (! name) ".length()"))
                 (char-range (lambda r (forall name
                                               (lambda (ch) (char-in ch (string-literal (apply string-append r))))))))
            body ...))])))

(define (guard fail-value expr)
  (string-append "if (not " (! expr) ")\n"
                 "  return " fail-value ";"))

(define (! s) (string-append "(" s ")"))

)
