
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

;;; TODO:
;;; * convert simplevars to parseobj
;;; * convert simplescm to parseobj
;;; * move all the old code out the way (including old base.scm functions)
;;; * commit to repo!!!!

;;; A simple lambda to Scheme compiler
;;;
;;; Front-end pipe on *all* files first:
;;; UTF8 -> | comments | indentation | -> SEXPR -> | parseobj | expand | -> PARSEOBJ
;;;
;;; Back-end pipe on just the main code file:
;;; PARSEOBJ -> | load | vars | betareduce | velcro | errsprint | output | -> SCHEME
;;;
;;; We like to run load first, because then there is
;;; no special behaviour given to this file over the included ones.
;;;
;;; It may be a good idea to make a cleverer "load" phase
;;; that knows how to run the front-end pipe on any input files it sees.
;;;
;;; It is definitely a good idea to make indentation reader itself return parseobjs,
;;; that way there is a direct mapping between raw source and output.
;;; No need for that at the comment phase, just strip out comments and
;;; leave everything else untouched.
;;;
;;; At the final pass the only top-level statements allowed are:
;;; * (__extern__ VAR)
;;; * a single expression of form: (__start__ E)
;;;   where E is one of (LAM, APP, VAR)
;;;   LAM = (__lambda__ VAR E) where E can contain VAR
;;;   APP = (E E)
;;;   VAR = VAR declared by an enclosing LAM, or by a (__extern__ VAR)

;; Useful symbols:
;; __lambda__ __define__ __decmacro__ __load__ __begin__ __extern__

;; Useful properties:
;; source-file source-start source-length

;; We should make __lambda__ and __define__ complain if they see a __foo__ shaped variable

;; I feel the direction we are heading in terms of code organisation,
;; is for each file to contain a single expression.
;;
;; This expression first takes 0 or more arguments, which are "module" definition maps.
;; The expression then returns a single "module" definition map.
;;
;; The configuration for which modules are put into the module arguments
;; could be provided outside of the source, essentially dependency injection.
;;
;; Perhaps the arguments to these file lambdas don't even have to be
;; "modules", they can just be whatever value is configured to be passed in.
;;
;; There could be multiple such lambdas per file.
;;
;; We can either come up with a way to uniquely name each,
;; or we can wrap them in a sort of mega module which contains the sub modules,
;; with the mega module being the only top-level lambda of the file.
;;
;; Personally I like the mega module idea.
;;
;; So now we are clear that we would each file to contain exactly one expression.
;;
;; Module imports should be provided as arguments to this expression.
;; It would be possible for the expression to call a function, A, which
;; returns the currect module to it, but then the problem is that A's
;; definition is not visible, so this is in no way prettier.
;;
;; So now we have files which take other files as arguments.
;; At the top, you have absolute primitives like Boolean
;; which take no arguments at all.
;;
;; Pair, is implemented using just a boolean like interface:
;; ("pair.smc" "boolean.smc") -> Pair
;;
;; Native functions are also passed in (perhaps with less fanfare).
;; ("basicio.smc" "nativeio.c/stdin" "nativeio.c/stdout" "nativeio.c/fgetb" "nativeio.c/fputb")
;;
;; Things like macros are *defined* inside a lambda's definition, and can
;; use the variables in the scope it lives in.
;;
;; I am still not sure how to return a macro, or otherwise make it visible to others.
;;
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
;; just so long as they are completely evaluated at compile-time?
;;
;; http://w210.ub.uni-tuebingen.de/dbt/volltexte/2006/2423/pdf/diss.pdf

(define (lambda-to-scm x)
  (if (not (= (length x) 3))
    "Lambda is wrong size"
    (let ((dec (cadr x))
          (ex  (caddr x)))
      (if (not (symbol? dec))
        "Invalid declare statement"
        (string-append "(lambda(" (symbol->string dec) ")" (expr-to-scm ex) ")")))))

(define (apply-to-scm x)
  (if (not (= (length x) 2))
    "Expression wrong size"
    (string-append "(" (expr-to-scm (car x)) " " (expr-to-scm (cadr x)) ")")))

(define (extern-to-scm x)
  (if (not (= (length x) 2))
    "Extern wrong size"
    (let ((tabl '((io#iocar  iocar)
                   (io#iocdr  iocdr)
                   (io#stdin  stdin)
                   (io#stdout stdout)
                   (io#fputb  fputb)
                   (io#fgetb  fgetb)
                   (io#int->church int->church)
                   (io#church->int church->int))))
      (let ((v (assoc-ref tabl (cadr x) #f)))
        (if (eq? v #f)
          "Extern not found"
          (symbol->string v))))))

(define (lookup-to-scm s)
  (if (not (symbol? s))
    "Lookup must be a symbol"
    (symbol->string s)))

(define (expr-to-scm x)
  (if (list? x)
    (let ((c (car x)))
      (cond
        ((eq? c '__lambda__) (lambda-to-scm x))
        ((eq? c '__extern__) (extern-to-scm x))
        (#t                  (apply-to-scm  x))))
    (lookup-to-scm x)))

(define preamble
"; iocons should only be used by native code
(define (iocons v z) (cons v z))
(define (iocar x) (car x))
(define (iocdr x) (cdr x))
(define (int->church i)
  (lambda (f)
    (lambda (x)
      (let loop ((k i) (c x))
        (if (= k 0) c (loop (- k 1) (f c)))))))
(define (church->int c) ((c (lambda (x) (+ x 1))) 0))
(define stdin (current-input-port))
(define stdout (current-output-port))
(define fgetb (lambda (f) (lambda (z) (iocons (char->integer (read-char f)) z))))
(define fputb (lambda (f) (lambda (c) (lambda (z) (iocons (write-char (integer->char c) f) z)))))
")

(define (simplescm-output p l)
  ; for now assume the last item is the expression
  (let* ((expr (cadr (p-last l)))
         (thecode (expr-to-scm expr)))
    (display (string-append preamble thecode) p)
    (newline p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (simplescm-output (current-output-port)
    (parse-strip-meta
      (read-sexprs-from-port (current-input-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
