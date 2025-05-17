(load "csm/expand.ss")

(import (csm expand))

(define expand-list
  '(("macro-test.ss.cpp" . "macro-test.cpp")))

(define cmd (command-line-arguments))

(define (expand-all)
  (for-each
    (lambda (x)
      (let* ((from (car x))
             (to (cdr x))
             (expanded (call-with-input-file from expand)))
        (with-output-to-file to
          (lambda () (printf "~a" expanded))
          'truncate)))
    expand-list))

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
   (expand-all)])
