
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

;;; merges file "foo"'s contents inito this one
;;; could also be called "include"
;;; (load "foo")

;;; allows the definitions of "foo" to be used within this file
;;; whilst not actually exposing the definitions outside
;;; (import "mod")
;;; for now import is not supported because it's harder to do

;;; It is an error to import/load the same definition from two different files
;;; We can perform all "load" operations, then fill in remaining symbols
;;; from imports

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (perform-loads px)
  (let ((x (parseobj-obj px)))
    (if (and (list? x) (>= (length x) 2))
      (let ((carx (parseobj-obj (car x))))
        (if (eq? carx '__load__)
          (let* ((fnmsym (parseobj-obj (cadr x)))
                  (filepx (parse-phase-load (read-sexprs-from-port (open-input-file (symbol->string fnmsym)))))
                  (filelpx (parseobj-obj filepx)))
            (parseobj-set
              (parseobj-mk
                (cons (parseobj-mk '__begin__ '()) filelpx)
                (parseobj-props px))
              'source-file
              fnmsym))
          (if (eq? carx '__begin__)
            ;; this does a (perform-loads '__base__) but thats harmless
            (parseobj-conv (lambda (x) (map perform-loads x)) px)
            px)))
      px)))

(define parse-phase-load (parseobj-convf (lambda (xs) (map perform-loads xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-load
      (read-sexprs-from-port (current-input-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
