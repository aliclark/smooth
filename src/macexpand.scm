
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

;;; This phase performs basic macro expansion.
;;; The current implementation is not very sophisticated and will eventually be replaced,
;;; but serves the basic needs (let, def, list, etc) well enough.

;; Current implementation:
;; 1) Macros are implemented directly in this file
;; 2) These can be used in the program after putting in (__decmacro__ themacroname)

;; Eventual implementation (see module.scm):
;; First class macros, with implementation directly in the source file:
;; (__macro__ (__lambda x x))

;; So, is it possible to have first class macros?
;; Do we want them? If we receive them via lambda argument, that
;; implies at least one enclosing lambda for which we cannot use a macro...
;; Or if we receive a package of macros, we can't use those macros,
;; to unpack the package.
;;
;; Perhaps it is best to say to that: create a standard pack of macros,
;; and make their definitions automatically referencable in the source.
;;
;; It is essentially equivalent to the compiler accepting top-level macros.
;;
;; http://matt.might.net/articles/metacircular-evaluation-and-first-class-run-time-macros/meta-circ.scmx
;; http://mainisusuallyafunction.blogspot.co.uk/2012/04/scheme-without-special-forms.html
;; https://en.wikipedia.org/wiki/Fexpr
;; https://en.wikipedia.org/wiki/Hygienic_macro
;;
;; Fexprs probably not a good idea. Or perhaps run-time macro type of things are allowed,
;; just so long as they are completely evaluated at compile-time? <- seems a good idea
;;
;; http://w210.ub.uni-tuebingen.de/dbt/volltexte/2006/2423/pdf/diss.pdf

;; gensyms will be random - if they collide simply recompile
;; Also, we can use parse metadata to indicate that a symbol is a gensym,
;; and teach varexpand and co to know they mustn't shadow, or be shadowed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macropobj x) (parseobj-mk x parseprops-null))

;; This runs into the age-old problem of referring to the wrong 'cons and the wrong 'nil
;; when they are shadowed.
;; We need to be able to refer to these more absolutely,
;; or even just copy their definitions into that position.
;;
;; The eventual implementation with first class macros should be able to solve this.
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

;; (comment ...) -> (__begin__)
(define (comment-macro args props)
  (parseobj-mk (list (macropobj '__begin__)) props))

;; (s ...) -> (__s__ ...)
(define (specialnym-macro sx args props)
  (let* ((s (parseobj-obj sx))
          (sp (parseobj-propsid sx))
          (syn (string->symbol (string-append "__" (symbol->string s) "__"))))
    (parseobj-mk (cons (parseobj-mk syn sp) args) props)))

;; (load "...") -> (__load__ ...)
(define (load-macro sx args props)
  (let* ((s (parseobj-obj sx))
          (sp (parseobj-propsid sx))
          (syn (string->symbol (string-append "__" (symbol->string s) "__")))
          (fstr (symbol->string (parseobj-obj (car args))))
          (fsym (string->symbol (string-append (substring fstr 1 (- (string-length fstr) 5)) ".smo"))))

    ;; FIXME: use fsym 
    (macropobj (list (macropobj '__load__) (macropobj fsym)))))

;    (parseobj-mk (cons (parseobj-mk syn sp) args) props)))

;; (let ((v x) ...) exp) -> ((__lambda__ v (let (...) exp)) x)
;; (let ()          exp) -> exp
(define (let-macro args props)
  (let ((vs (parseobj-obj (car args)))
         (vp (parseobj-propsid (car args))))
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

;; The important thing is that if "(lambda ...)" is has offset 33, length 15 in the source program,
;; the resulting "(lambda ...)" should have those properties (even if the source length would be different).
;; It would then be clear that this came from a macroexpansion.
;; We could even add our own metadata property, a stack trace of expansions:
;; '(macexpansions ((0 (lambda (x y) exp)) (1 (intermediate-macexp))))
;; So if one macro expands to another and so on, we can see what macros were involved.
;; This may be overkill, since we can always reproduce this once we have identified the source location.
;;
;; (lambda (x ...) exp) -> (__lambda__ x (lambda (...) exp))
;; (lambda ()      exp) -> exp
(define (lambda-macro args props)
  (let ((al (parseobj-obj (car args)))
         (ap (parseobj-propsid (car args))))
    (if (null? al)
      (cadr args)
      (parseobj-mk
        (list (macropobj '__lambda__) (car al)
          (macropobj
            (list (macropobj 'lambda) (parseobj-mk (cdr al) ap) (cadr args))))
        props))))

;; FIXME: We must ensure both define and lambda are visible
;;
;; (define (f ...) exp) -> (__define__ f (lambda (...) exp))
;; (define f exp)       -> (__define__ f exp)
(define (def-macro args props)
  (let ((v (parseobj-obj (car args)))
         (vp (parseobj-propsid (car args))))
    (parseobj-mk
      (if (list? v)
        (list (macropobj '__define__)
          (car v)
          (macropobj
            (list (macropobj 'lambda) (parseobj-mk (cdr v) vp) (cadr args))))
        (cons (macropobj '__define__) args))
      props)))

(define (expand px)
  (let* ((x (parseobj-obj px))
          (props (parseobj-propsid px))
          (mac (parseobj-obj (car x)))
          (args (cdr x)))
    (cond
      ((eq? mac 'comment)
        (comment-macro args props))
      ((or (eq? mac 'decmacro) (eq? mac 'extern))
        (specialnym-macro (car x) args props))
      ((eq? mac 'load)   (load-macro (car x) args props))
      ((eq? mac 'let)    (let-macro  args props))
      ((eq? mac 'lambda) (lambda-macro   args props))
      ((eq? mac 'define) (def-macro  args props))
      (#t (display 'macro-not-found)))))

(define (ismacrosym? x)
  (and (symbol? x)
    (or
      (eq? x 'comment)
      (eq? x 'decmacro)
      (eq? x 'load)
      (eq? x 'extern)
      (eq? x 'list)
      (eq? x 'let)
      (eq? x 'lambda)
      (eq? x 'define))))

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
              (parseobj-propsid px)))
          ((eq? c '__begin__)
            (parseobj-mk
              (cons (car x) (map macexpand (cdr x)))
              (parseobj-propsid px)))
          ((reserved-symbol-obj? px)
            px)
          ((= (length x) 2)
            (parseobj-mk
              (list (macexpand (car x)) (macexpand (cadr x)))
              (parseobj-propsid px)))
          (#t (begin (display-error "unknown expression size") px))))
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
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (parse-phase-filter (cadr pxl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
