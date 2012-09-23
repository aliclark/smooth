
;(load "lambda.scm")
;(load "bool.scm")

(define pair            rsd3)
(define fst             (applies true))
(define snd             (applies false))
(define (curry   f) (lambda (a) (lambda (b) (f ((pair a) b)))))
(define (uncurry f)     (((fgh f) fst) snd))
