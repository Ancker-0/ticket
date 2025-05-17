(library (csm)
(export op ops ! flatten
        begins genname str+ string-literal
        forall forsome)
(import (chezscheme))

(define (! s) (string-append "(" s ")"))

(define (flatten l)
  (cond
    [(not (list? l)) (list l)]
    [(null? l) l]
    [else
     (let ((a (car l))
           (d (cdr l)))
       (append (flatten a) (flatten d)))]))

(define (op name)
  (lambda (a b)
    (! (string-append (! a) name (! b)))))

(define (ops name)
  (lambda (a . r)
    (apply string-append `("(" ,(! a) ,@(flatten (map (lambda (x) (list " " name " " (! x))) r)) ")"))))

(define (begins . args)
  (apply string-append 
         `("({\n"
           ,@(flatten (map (lambda (x) (list x "\n")) args))
           "})")))

(define str+ string-append)

(define genname
  (let ((counter 0))
    (lambda r
      (set! counter (1+ counter))
      (let ((name (if (null? r)
                 (substring (gensym->unique-string (gensym)) 0 5)
                 (car r))))
        (string-append
          name
          (number->string counter))
        ))))

(define (string-literal s)
  (define (escape ch)
    (case ch
      [#\" "\\\""]
      [#\\ "\\\\"]
      [#\newline "\\n"]
      [else (string ch)]))
  (apply string-append `("\"" ,@(map escape (string->list s)) "\"")))

(define (forall x y)
  (let ((name (genname "tmpvar"))
        (flag (genname "flag")))
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
  (let ((name (genname "tmpvar"))
        (flag (genname "flag")))
    (begins
      "bool " flag " = false;"
      (string-append "for (auto &" name " : " (! x) ")\n"
                     "  if (" (y name) ") {\n"
                     "    " flag " = true;\n"
                     "    break;\n"
                     "  }")
      flag ";"
      )))

)


#!eof

(let ()
  (import (csm))
  (define and (ops "&&"))
  (define (<= a b)
    (! (string-append (! a) "<=" (! b))))
  (define > (op ">"))
  (define == (op "=="))
  (display
    (and (<= "1" "2")
         "flag == true"
         (or "1 + 1 == 2"
             "2")))
  )
