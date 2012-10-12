
(load "base.scm")

;; TODO: It would also be nice to apply against any lambdas which return an argument
;; since there is no risk of non-termination there.

(define counter 0)
(define (mygensym)
  (let ((c counter))
    (set! counter (+ counter 1))
    (string->symbol (string-append "__g" (number->string c) "__"))))

(define (reduce-full-end px x fx)
  (parseobj-mk
    (list fx (reduce-form (cadr x)))
    (parseobj-propsid px)))

(define (reduce-apply px)
  (let ((x (parseobj-obj px)))

      (let* ((fx (reduce-form (car x))))

        (let ((f (parseobj-obj fx)))
          (if (list? f)

            (if (reserved-form-type? fx '__extern__ 2)

              (reduce-full-end px x fx)

              (if (reserved-form-type? fx '__lambda__ 3)
                (reduce-form (subst (caddr f) (parseobj-obj (cadr f)) (cadr x)))

                ;; the expression is fully reduced and is still an application.
                (reduce-full-end px x fx)))

            ; I'm not sure this can even occur - calling a lambda var
            (reduce-full-end px x fx))))))

(define (reduce-form px)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        px
        (if (reserved-form-type? px '__extern__ 2)
          px
          (reduce-apply px)))
      px)))

(define parse-phase-reduce
  (parseobj-convf
    (lambda (xs)
      (map
        (lambda (px)
          (if (reserved-form-type? px '__start__ 2)
            (parseobj-sel 1 reduce-form px)
            px))
        xs))))

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-reduce
      (read-sexprs-from-port (current-input-port)))))
