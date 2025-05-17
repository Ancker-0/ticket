(load "csm/expand.ss")

(import (csm expand))

(define expand-list
  '(("macro-test.ss.cpp" . "macro-test.cpp")))

(for-each
  (lambda (x)
    (let* ((from (car x))
           (to (cdr x))
           (expanded (call-with-input-file from expand)))
      (with-output-to-file to
        (lambda () (printf "~a" expanded))
        'truncate)))
  expand-list)
