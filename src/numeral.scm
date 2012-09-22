
(load "lambda.scm")
(load "bool.scm")

(define n0               drop)

(define (++      n) (lambda (f) (lambda (x) (f ((n f) x)))))
(define (+     m) (lambda (n) (lambda (f) (lambda (x) ((n f) ((m f) x))))))
(define *               fcompose)
(define **              applies)
(define (--      n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (const x)) id))))
(define (-     m) (lambda (n)     ((n --) m)))
(define square          (fdup *))
(define ^2              square)


(define (zero?   n)     ((n (const false)) true))
(define (zero? n) ((n (lambda (x) false)) true))

(define (<=    m) (lambda (n)     (zero? ((- m) n))))
(define (>     m) (lambda (n)     (not ((<= m) n))))
(define (=     m) (lambda (n)     ((fand ((<= m) n)) ((<= n) m))))
(define (>=    m) (lambda (n)     ((for ((> m) n)) ((= m) n))))
(define (<     m) (lambda (n)     (not ((>= m) n))))
(define (!=    m) (lambda (n)     (not ((= m) n))))
(define <>              !=)
(define n0?              zero?)

(define n1               (++ n0))
(define n2               (++ n1))
(define n3               (++ n2))
(define n4               (++ n3))
(define n5               (++ n4))
(define n6               (++ n5))
(define n7               (++ n6))
(define n8               (++ n7))
(define n9               (++ n8))
(define n10              (++ n9))

(define +gh             (fgh +))
(define *gh             (fgh *))
