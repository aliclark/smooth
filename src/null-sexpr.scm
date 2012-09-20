
(load "base.scm")

(define (start)
  (parse-output-to-port (current-output-port)
    (read-sexprs-from-port (current-input-port))))
