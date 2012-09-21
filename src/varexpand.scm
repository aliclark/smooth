
;;;; Copyright (c) 2009, Ali Clark <emailaliclark@gmail.com>
;;;;
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(load "base.scm")

;;; This phase takes a file containing __define__ forms and returns the
;;; definitions of one of the symbols defined, after replacing symbols
;;; within the code with their values.
;;;
;;; (__define__ foo  (lambda x x))
;;; (__define__ main (foo foo))
;;;
;;; would convert to:
;;;
;;; (__start__ ((__lambda__ x x) (__lambda__ x x)))
;;;
;;; Obviously this will not work if a symbol's value references itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main-symbol 'main)

(define (reduce-value internal-defines shadows px)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        (parseobj-sel 2
          (lambda (y)
            (reduce-value internal-defines (ensure-contains (parseobj-obj (cadr x)) shadows) y))
          px)
        (if (reserved-form-type? px '__extern__ 2)
          px
          (parseobj-mk
            (p-map (lambda (z) (reduce-value internal-defines shadows z)) x)
            (parseobj-props px))))
      (if (contains? shadows x)
        px
        (let ((val (assoc-ref internal-defines x #f)))
          (if (eq? val #f)
            (begin (display (list 'unbound 'var_ x)) #f)
            (reduce-value internal-defines p-null val)))))))

(define (assoc-append x1 x2)
  (if (null? x1)
    x2
    (assoc-append (cdr x1) (cons (car x1) x2))))

(define (grab-defines xs)
  (foldr
    (lambda (px acc)
      (let ((x (parseobj-obj px)))
        (if (reserved-form-type? px '__define__ 3)
          (assoc-set acc (parseobj-obj (cadr x)) (p-third x))
          (if (reserved-form-type? px '__begin__ 0)
            (assoc-append acc (grab-defines (cdr (parseobj-obj px))))
            acc))))
    '()
    xs))

(define parse-phase-filter
  (parseobj-convf
    (lambda (xs)
      (let ((internal-defines (grab-defines xs)))
        (let ((m (assoc-ref internal-defines main-symbol #f)))
          (if (equal? m #f)
            (parse-error
              (string-append "Main symbol (" (symbol->string main-symbol) ") not present"))
            (list
              (parseobj-mk
                (list (parseobj-mk '__start__ '())
                  (reduce-value internal-defines p-null m))
                '()))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-filter
      (read-sexprs-from-port (current-input-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
