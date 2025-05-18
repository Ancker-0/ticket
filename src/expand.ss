(load "csm/expand.ss")

(import (csm expand))

(define expand-list
  '(("macro-test.ss.cpp" . "macro-test.cpp")
    ("typedecl.ss.cpp" . "typedecl.cpp")))

(define expand-group-list
  '((("macro-test.ss.cpp" . "macro-test.cpp") )
    (("typedecl.ss.cpp" . "typedecl.cpp") )))

(define cmd (command-line-arguments))

(define (clang-format name)
  (system (format "clang-format -i ~s --style=Google" name)))

(define (expand-all)
  (for-each
    (lambda (x)
      (let* ((from (car x))
             (to (cdr x))
             (expanded (call-with-input-file from expand)))
        (with-output-to-file to
          (lambda () (printf "~a" expanded))
          'truncate)
        (clang-format to)))
    expand-list))

(define (expand-group)
  (define (expand-list/env lst env)
    (for-each
      (lambda (x)
        (let* ((from (car x))
               (to (cdr x))
               (expanded (call-with-input-file from (lambda (port) (expand port env)))))
          (with-output-to-file to
            (lambda () (printf "~a" expanded))
            'truncate)
          (clang-format to)))
      lst))
  (for-each
    (lambda (lst)
      (expand-list/env
        lst
        (copy-environment (environment '(chezscheme)))))
    expand-group-list))

(define (clean-all)
  (for-each
    (lambda (x)
      (let* ((from (car x))
             (to (cdr x)))
        (delete-file to)))
    expand-list))

(cond
  [(null? cmd)
   (fprintf (current-error-port) "Unexpected argument.\n")
   (exit #f)]
  [(equal? (car cmd) "clean")
   (clean-all)]
  [(equal? (car cmd) "expand")
   (expand-all)]
  [(equal? (car cmd) "group")
   (expand-group)])
