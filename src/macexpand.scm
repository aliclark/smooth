
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

;;; This phase performs macro expansion
;;; (expand macname [arg1 ...])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gensyms are random - if they collide simply recompile!

(define (macropobj x) (parseobj-mk x '()))

;; This runs into the age-old problem of referring to the wrong 'cons and the wrong 'nil
;; when they are shadowed.
;; We need to be able to refer to these more absolutely,
;; or even just copy their definitions into that position
;;
;; FIXME: It is our responsibility to supply a definition
;; for cons and nil
;;
;; (list x ...) -> (cons x (list ...))
;; (list)       -> nil
(define (list-macro args props)
  (parseobj-mk
    (if (null? args)
      'nil
      (list (macropobj (list (macropobj 'cons) (car args)))
        (macropobj (cons (macropobj 'list) (cdr args)))))
    props))

;; (comment ...) -> (begin)
(define (comment-macro args props)
  (parseobj-mk (list (macropobj '__begin__)) props))

;; (s ...) -> (__s__ ...)
(define (specialnym-macro sx args props)
  (let* ((s (parseobj-obj sx))
          (sp (parseobj-props sx))
          (syn (string->symbol (string-append "__" (symbol->string s) "__"))))
    (parseobj-mk (cons (parseobj-mk syn sp) args) props)))

;; (let ((v x) ...) exp) -> ((lambda v (let (...) exp)) x)
;; (let ()          exp) -> exp
(define (let-macro args props)
  (let ((vs (parseobj-obj (car args)))
         (vp (parseobj-props (car args))))
    (if (null? vs)
      (cadr args)
      (let ((a (parseobj-obj (car vs))))
        (parseobj-mk
          (list
            (macropobj
              (list (macropobj '__lambda__) (car a)
                (macropobj (list (macropobj 'let)
                             (parseobj-mk (cdr vs) vp)
                             (cadr args)))))
            (cadr a))
          props)))))

;; The important thing is that if "(fn ...)" is has offset 33, length 15 in the source program,
;; the resulting "(lambda ...)" should have those properties (even if the source length would be different).
;; It would then be clear that this came from a macroexpansion.
;; We could even add our own metadata property, a stack trace of expansions:
;; '(macexpansions ((0 (fn (x y) exp)) (1 (intermediate-macexp))))
;; So if one macro expands to another and so on, we can see what macros were involved.
;; This may be overkill, since we can always reproduce this once we have identified the source location.
;;
;; (fn (x ...) exp) -> (lambda x (fn (...) exp))
;; (fn ()      exp) -> exp
(define (fn-macro args props)
  (let ((al (parseobj-obj (car args)))
         (ap (parseobj-props (car args))))
    (if (null? al)
      (cadr args)
      (parseobj-mk
        (list (macropobj '__lambda__) (car al)
          (macropobj
            (list (macropobj 'fn) (parseobj-mk (cdr al) ap) (cadr args))))
        props))))

;; FIXME: We must ensure both define and fn are visible
;;
;; (def (f ...) exp) -> (define f (fn (...) exp))
;; (def f exp)       -> (define f exp)
(define (def-macro args props)
  (let ((v (parseobj-obj (car args)))
         (vp (parseobj-props (car args))))
    (parseobj-mk
      (if (list? v)
        (list (macropobj '__define__)
          (car v)
          (macropobj
            (list (macropobj 'fn) (parseobj-mk (cdr v) vp) (cadr args))))
        (cons (macropobj '__define__) args))
      props)))

(define (expand px)
  (let* ((x (parseobj-obj px))
          (props (parseobj-props px))
          (mac (parseobj-obj (car x)))
          (args (cdr x)))
    (cond
      ((eq? mac 'comment)
        (comment-macro args props))
      ((or (eq? mac 'load) (eq? mac 'decmacro) (eq? mac 'decextern))
        (specialnym-macro (car x) args props))
      ((eq? mac 'let)  (let-macro  args props))
      ((eq? mac 'fn)   (fn-macro   args props))
      ((eq? mac 'def)  (def-macro  args props))
      (#t (display 'macro-not-found)))))

(define (ismacrosym? x)
  (and (symbol? x)
    (or
      (eq? x 'comment)
      (eq? x 'decmacro)
      (eq? x 'load)
      (eq? x 'decextern)
      (eq? x 'list)
      (eq? x 'let)
      (eq? x 'fn)
      (eq? x 'def))))

(define (macexpand px)
  (let ((x (parseobj-obj px)))
    (if (and (list? x) (>= (length x) 1))
      (let ((c (parseobj-obj (car x))))
        (cond
          ((ismacrosym? c)
            (macexpand (expand px)))
          ((or (eq? c '__lambda__) (eq? c '__define__))
            (parseobj-mk
              (list (car x) (cadr x) (macexpand (caddr x)))
              (parseobj-props px)))
          ((eq? c '__begin__)
            (parseobj-mk
              (cons (car x) (map macexpand (cdr x)))
              (parseobj-props px)))
          ((= (length x) 2)
            (parseobj-mk
              (list (macexpand (car x)) (macexpand (cadr x)))
              (parseobj-props px)))
          (#t px)))
      px)))

;; TODO: some sort of validation
;; beware: if another macro expands into a __decmacro__
;; statement... then we have more macros to worry about.
(define (determine-all-valid-macronames x)
  (if (not (parseobj-has-decspecial? x '__decmacro__))
    '()
    (parseobj-strip-meta x)))

(define parse-phase-filter
  (parseobj-convf
    (lambda (x)
      (map macexpand x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-filter
      (read-sexprs-from-port (current-input-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
