
(load "base.scm")

;; If the lambda is contained (not a closure) it can be
;; compared with any other contained expression.

;; If a lambda is a closure (references variables at higher scopes),
;; then any variables at higher scopes must be the same ones
;; for any two lambda expressions to be equal.
;; If the depth of the closure changes with respect to the closed variables,
;; then this will not cause a problem for Scheme, but may do for C.
;;
;; eg (lamba (3 0)) and (lambda (2 0)) where the former is
;; referring to the same closed variable as the latter,
;; but has higher index because it is within an extra lambda.

;; (lambda x (lambda y (somenative (lambda z (z x y)) y)))
;;   leads to:
;; (lambda (lambda (somenative (lambda (0 1 2)) 1)))

;; Look through with an assoc-list of (varname count)
;; whenever a lambda is encountered, add it with count 0,
;; replacing a previous count if it shadows, map the other counts doing +1,
;; and call recursively inside with this new assoc-list.
;; When the call ends this assoc-list argument is naturally discarded.

(define (debrujin px vari)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        (parseobj-sel 1
          (lambda (v) (parseobj-mk '__debrujin__ (parseobj-propsid v)))
          (parseobj-sel 2
            (lambda (exp)
              (debrujin exp
                (cons (list (parseobj-obj (cadr x)) 0)
                  (map (lambda (p) (list (car p) (+ (cadr p) 1))) vari))))
            px))
        (if (reserved-symbol-obj? px)
          px
          (parseobj-mk (map (lambda (x) (debrujin x vari)) x) (parseobj-propsid px))))
      (parseobj-mk (number->symbol (assoc-ref vari x #f)) (parseobj-propsid px)))))

(define parse-phase-debrujin
  (parseobj-convf
    (lambda (xs)
      (map
        (lambda (px)
          (if (reserved-form-type? px '__start__ 2)
            (parseobj-sel 1 (lambda (x) (debrujin x '())) px)
            px))
        xs))))

(define (start)
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (parse-phase-debrujin (cadr pxl)))))
