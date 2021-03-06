
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
;;; (__start__ ((__lambda__ foo (foo foo)) (__lambda__ x x)))
;;;
;;; Obviously this will not work if a symbol's value references itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main-symbol 'main)

(define (find-lets internal-defines shadows px indefs)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)

        (find-lets internal-defines (ensure-contains (parseobj-obj (cadr x)) shadows) (caddr x) indefs)

        (if (reserved-form-type? px '__extern__ 2)
          '()
          (apply append (p-map (lambda (z) (find-lets internal-defines shadows z indefs)) x))))

      (if (contains? shadows x)
        '()

        (if (contains? indefs x)
          (begin (display x) (newline) 'error-in-definition-of-this-symbol)

          (let ((val (assoc-ref internal-defines x #f)))
            (if (eq? val #f)
              (begin
                (die-gnose "Variable not defined: " px)
                #f)

              (cons x (find-lets internal-defines p-null val (cons x indefs))))))))))

;; Must only remove subsequent duplicates
(define (remove-duplicates-inner xs seen)
  (if (null? xs)
    '()
    (let ((x (car xs)))
      (if (contains? seen x)
        (remove-duplicates-inner (cdr xs) seen)
        (cons x (remove-duplicates-inner (cdr xs) (cons x seen)))))))

(define (remove-duplicates xs) (remove-duplicates-inner xs '()))

(define (get-defines tab vars) (map (lambda (v) (list v (assoc-ref tab v #f))) vars))

(define (reduce-value internal-defines shadows px indefs)
  (let ((vs (get-defines internal-defines (remove-duplicates (reverse (find-lets internal-defines shadows px indefs))))))
    (make-letex vs px)))

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

;; this will still leave empty begin forms behind which is a bit ugly
(define (remove-defines xs)
  (map
    (lambda (x)
      (if (reserved-form-type? x '__begin__ 0)
        (parseobj-conv remove-defines x)
        x))
    (filter (lambda (x) (not (reserved-form-type? x '__define__ 3))) xs)))

(define parse-phase-filter
  (parseobj-convf
    (lambda (xs)
      (let ((internal-defines (grab-defines xs)))
        (let ((m (assoc-ref internal-defines main-symbol #f)))
          (if (equal? m #f)
            (parse-error
              (string-append "Main symbol (" (symbol->string main-symbol) ") not present"))

            (cons
              (parseobj-mk
                (list (parseobj-mk '__start__ parseprops-null)
                  (reduce-value internal-defines p-null m (list main-symbol)))
                parseprops-null)

              (remove-defines xs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (parse-phase-filter (cadr pxl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
