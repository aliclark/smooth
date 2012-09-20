#!/usr/bin/gsi-script

(define (runmod)
  (let ((c (command-line)))
    (if (< (length c) 2)
      (begin
        (display "Nothing to do")
        (newline))
      (begin
        (load (cadr c))
        ; all input must be read from stdin
        (start)))))

(runmod)
