
(load "../smoothlang/anc2020/lambda.smc")
(load "../smoothlang/anc2020/bool.smc")
(load "../smoothlang/anc2020/pair.smc")

(define (cons h) (lambda (t) ((pair false) ((pair h) t))))
(define nil        ((fdup pair) true))
(define null       fst)
(define null?      null)
(define sat        ((fcompose not) null))
(define head       ((fcompose fst) snd))
(define tail       ((fdup fcompose) snd))
(define car        head)
(define cdr        tail)
(define cadr       ((fcompose car) cdr))
(define caddr      ((fcompose cadr) cdr))

(define (equal a) (lambda (b)
  ((or ((and (null a)) (null b)))
    ((((and4 (sat a)) (sat b)) ((equal (head a)) (head b))) ((equal (tail a)) (tail b))))))

(define (foldr f) (lambda (z) (lambda (xs) (((null? xs) z)   ((f (head xs)) (((foldr f) z) (tail xs)))))))
(define (map f) (lambda (xs)     (((null? xs) xs)  ((cons (f (head xs))) ((map f) (tail xs))))))
(define (append xs) (lambda (ys) (((null? xs) ys)  ((cons (head xs)) ((append (tail xs)) ys)))))
(define (take n) (lambda (xs)	   (((zero? n)  nil) ((cons (head xs)) ((take (-- n)) (tail xs))))))
(define (reverse) (lambda (xs)   (((null? xs) nil) ((append (reverse (tail xs))) ((cons (head xs)) nil)))))

(define (last l)    (((null? (tail l)) (head l)) (last (tail l))))
(define (butlast l) (((null? (tail l)) nil)      ((cons (head l)) (butlast (tail l)))))
