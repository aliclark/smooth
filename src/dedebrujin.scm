
(load "base.scm")

;; if we were to allow intermixing of debrujin indices and normal lambdas on the input,
;; what happens if a normal lambda tries to redefine variable 0 ?
;;
;; for now, just assume all lambdas are in debrujin form

(define (symbol->number x) (string->number (symbol->string x)))

(define counter 0)
(define (mygensym)
  (let ((c counter))
    (set! counter (+ counter 1))
    (string->symbol (string-append "__x" (number->string c) "__"))))

(define (dedebrujin px vari)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        (if (eq? (parseobj-obj (cadr x)) '__debrujin__)
          (let ((sym (mygensym)))
            (parseobj-sel 1
              (lambda (v) (parseobj-mk sym (parseobj-propsid v)))
              (parseobj-sel 2
                (lambda (exp)
                  (dedebrujin exp
                    (cons (list 0 sym)
                      (map (lambda (p) (list (+ (car p) 1) (cadr p))) vari))))
                px)))
          'error-not-debrujin)
        (if (reserved-symbol-obj? px)
          px
          (parseobj-mk (map (lambda (x) (dedebrujin x vari)) x) (parseobj-propsid px))))
      (parseobj-mk (assoc-ref vari (symbol->number x) #f) (parseobj-propsid px)))))

(define parse-phase-dedebrujin
  (parseobj-convf
    (lambda (xs)
      (map
        (lambda (px)
          (if (reserved-form-type? px '__start__ 2)
            (parseobj-sel 1 (lambda (x) (dedebrujin x '())) px)
            px))
        xs))))

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-dedebrujin
      (read-sexprs-from-port (current-input-port)))))
