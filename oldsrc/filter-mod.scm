#!/usr/bin/gsi-script

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

;;; This phase takes a file containing (()define) forms and returns the
;;; definitions of one of the symbols defined, after replacing symbols
;;; within the code with their values.
;;;
;;; (define foo  (lambda x x))
;;; (define main (foo foo))
;;;
;;; would convert to:
;;;
;;; ((lambda x x) (lambda x x))
;;;
;;; Obviously this will not work if a symbol's value references itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ensure-contains cons)

(define (contains? l x)
  (if (null? l) false (if (equal? (head l) x) true (contains? (tail l) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main-symbol 'main)

(define internal-defines (make-table))

(define (reduce-value shadows p)
  (cond
    ((lambda-expression? p)
      (lambda-expression-mk (lambda-expression-var p)
        (reduce-value (ensure-contains (lambda-expression-var p) shadows)
          (lambda-expression-body p))))
    ((non-resvd-list? p) (p-map (partial reduce-value shadows) p))
    ((p-symbol? p)
      (if (contains? shadows p) p
        (reduce-value p-null (table-ref internal-defines p))))
    (else p)))

(define (parse-phase-filter xs)
  (map
    (lambda (x)
      (if (reserved-form-type? x 'define)
        (table-set! internal-defines (p-second x) (p-third x))
        x))
    xs)
  (let ((m (table-ref internal-defines main-symbol #f)))
    (if (equal? m #f)
      (parse-error
        (string-append "Main symbol (" (symbol->string main-symbol) ") not present"))
      (list (reduce-value p-null m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)

  (parse-output-to-port (current-output-port)

    (parse-phase-filter

      (parse-strip-meta
        (parse-check-errors (current-error-port)
          (parse-input-from-port (current-input-port)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

