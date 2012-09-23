
(load "base.scm")

(define (reduce-form px)
  px)

(define parse-phase-reduce
  (parseobj-convf
    (lambda (px)
      (if (reserved-form-type? px '__start__ 2)
        (parseobj-sel-conv 1 reduce-form px)
        px))))

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-reduce
      (read-sexprs-from-port (current-input-port)))))
