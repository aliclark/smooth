
(load "base.scm")

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-strip-meta
      (read-sexprs-from-port (current-input-port)))))
