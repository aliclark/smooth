
(load "base.scm")

(define counter 0)
(define (mygensym)
  (let ((c counter))
    (set! counter (+ counter 1))
    (string->symbol (string-append "__g" (number->string c) "__"))))

; replace all occurrences of v in exp with arg,
; except those shadowed by a lambda
(define (subst exp v arg)
  (let ((x (parseobj-obj exp)))
    (if (list? x)
      (if (reserved-form-type? exp '__extern__ 2)
        exp
        (if (reserved-form-type? exp '__lambda__ 3)
          (let ((s (parseobj-obj (cadr x))))
            (if (eq? s v)
              exp
              (parseobj-sel 2 (lambda (b) (subst b v arg)) exp)))
          (parseobj-mk (map (lambda (y) (subst y v arg)) x)
            (parseobj-props exp))))
      (if (eq? x v)
        arg
        exp))))

(define (reduce-end px x fx)
  (parseobj-mk
    (list fx (cadr x))
    (parseobj-props px)))

;; Using the following reduction will potentially not terminate.
(define (reduce-full-end px x fx)
  (parseobj-mk
    (list fx (reduce-form (cadr x)))
    (parseobj-props px)))

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
