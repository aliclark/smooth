
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
;;; 1) Grow Scheme and Smooth towards each other.
;;;   > Implement more Scheme-like language features (pretty much done now).
;;;   > Use a progressively simpler subset of Scheme in implementing the passes.
;;; 2) Make/keep the compiler implementation reasonably performant.
;;; 3) Improve the performance of the generated Scheme code (use betareduce especially).
;;;
;;; Once the above conditions are met, then it is sensible to
;;; switch the implementation to being thought of as a Smooth program.
;;; There is no point doing this if it is still very slow.
;;;
;;; 4) Start building out Smooth past Scheme - velcro, modules, macros
;;;
;;;
;;; A simple lambda to Scheme compiler
;;;
;;; Front-end pipe on *all* files first:
;;; UTF8 -> | comments | indentation | -> SEXPR -> | parseobj | expand | -> PARSEOBJ
;;;
;;; Back-end pipe on just the main code file:
;;; PARSEOBJ -> | load | vars | dearitise | betareduce | velcro | errsprint | rearitise | output | -> SCHEME
;;;
;;; We like to run load first, because then there is
;;; no special behaviour given to this file over the included ones.
;;;
;;; It may be a good idea to make a cleverer "load" phase
;;; that knows how to run the front-end pipe on any input files it sees.
;;; However, given that "load" will be replaced at some point, this is moot.
;;;
;;; It is definitely a good idea to make indentation reader itself return parseobjs,
;;; that way there is a direct mapping between raw source and output.
;;; No need for that at the comment phase, just strip out comments and
;;; leave everything else untouched.
;;;
;;; At the final pass the only top-level statements allowed are:
;;; * a single expression of form: (__start__ E)
;;;   where E is one of (LAM, APP, VAR, EXT)
;;;   LAM = (__lambda__ VAR E) where E can contain VAR
;;;   APP = (E E)
;;;   VAR = VAR declared by an enclosing LAM
;;;   EXT = (__extern__ someidentifier)

;; Useful symbols:
;; __lambda__ __begin__ __extern__
;;
;; Useful for porting from Scheme, but deprecated:
;; __define__ __decmacro__ __load__
;;
;; Will be useful soon:
;; __import__ __macro__

;; Useful properties:
;; source-file source-start source-length

;; We should make __lambda__ and __define__ complain if they are
;; told to declare a __foo__ shaped variable

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
    (begin (display-error "Expression wrong size") (parse-output-to-port (current-error-port) x) "---ERROR---")
    (string-append "(" (expr-to-scm (car x)) " " (expr-to-scm (cadr x)) ")")))

(define (extern-to-scm x)
  (if (not (= (length x) 2))
    "Extern wrong size"
    (let ((tabl '((io_iocons  io_iocons)
                   (io_iocar  io_iocar)
                   (io_iocdr  io_iocdr)
                   (io_stdin  io_stdin)
                   (io_stdout io_stdout)
                   (io_fputb  io_fputb)
                   (io_fgetb  io_fgetb)
                   (io_inttochurch io_inttochurch)
                   (io_churchtoint io_churchtoint))))
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
"(define (io_iocons v) (lambda (z) (cons v z)))
(define (io_iocar x) (car x))
(define (io_iocdr x) (cdr x))
(define (io_inttochurch i)
  (lambda (f)
    (lambda (x)
      (let loop ((k i) (c x))
        (if (= k 0) c (loop (- k 1) (f c)))))))
(define (io_churchtoint c) ((c (lambda (x) (+ x 1))) 0))
(define io_stdin (current-input-port))
(define io_stdout (current-output-port))
(define io_fgetb (lambda (f) (lambda (z)
                               (let* ((c (read-char f))
                                        ; for the very time being we can get away
                                       ; with using 0 instead of -1 for EOF
                                       (n (if (eof-object? c) 0 (char->integer c))))
                                 ((io_iocons n) z)))))
(define io_fputb (lambda (f) (lambda (c) (lambda (z) ((io_iocons (write-char (integer->char c) f)) z)))))
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
