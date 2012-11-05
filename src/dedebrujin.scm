
(load "base.scm")

;; We allow intermixing of debrujin and normal format.
;; If a numeric-looking variable is defined, result is undefined.
;;
;; When counting debrujin depth, only the debrujin lambdas count.

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

          (parseobj-sel 2
            (lambda (exp) (dedebrujin exp vari))
            px))

        (if (reserved-symbol-obj? px)

          ;;; FIXME: ugly !!!! chnage to proper gensyms.
          (if (string=? (substring (symbol->string (parseobj-obj (car x))) 0 3) "__d")
            (parseobj-mk (map (lambda (x) (dedebrujin x vari)) x) (parseobj-propsid px))

            px)

          (parseobj-mk (map (lambda (x) (dedebrujin x vari)) x) (parseobj-propsid px))))

      (if (reserved-symbol-sym? px)
        px
        (parseobj-mk (assoc-ref vari (symbol->number x) #f) (parseobj-propsid px))))))

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
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (parse-phase-dedebrujin (cadr pxl)))))
