
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

(load "front-end.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; callers
;;
;; This style of function takes a flatx, startt and inl and returns
;; (list precommands item tmps copyrefs)
;;
;; flatx is a flat list of (func args...)
;; startt is the number to start taking tmps from
;; inl is whether or not we are in a lambda (and thus need to do closures)
;;
;; Where precommands is a list in order of lines to run firt
;; item is the embeddable value representing the expression
;; and tmps is the number of temporary variable used in the expression.
;; copyrefs is the list of all things that are used
;; possibly multiple entries for a value

;
; Note - the closure_create version needs to know how many arguments
; its code value will take
;

(define (itemstr- x id suppress)
  (cond
    ((symbol? x) (symbol->string x))
    ((string? x) x)
    ((number? x) (number->string x))
    ((tmpref? x) (string-append "tmp[" (number->string (tmpref-id x)) "]"))
    ((varref? x) (string-append "VARIABLE_LOOKUP(" (number->string (varref-val x)) ")"))
    ((localref? x) (string-append "locals[" (number->string (localref-id x)) "]"))
    ((lamref? x)
      (if (lamref-is-closure? x)
        (string-append "closure_single(LAMBDA(" (number->string (lamref-id x)) "), VARIABLE_LOOKUP(0))")
        (string-append "LAMBDA(" (number->string (lamref-id x)) ")")))
    ((preval? x) (string-append "preval[" (number->string (preval-id x)) "]"))
    ((primop-symbol? x) (primop-to-id x))
    ((non-resvd-list? x)
      (string-append id (itemstr- (car x) id true) "(" (implode ", " (map (lambda (y) (itemstr- y "" true)) (cdr x)))
        (if suppress ")" ");\n")))
    (else (pretty-print x))))

(define (itemstr x id) (itemstr- x id false))

(define (cstr-argexprs args startt)
  (if (null? args)
    (list (list) (list) startt (list) startt)
    (let ((item (car args)))
      (if (non-resvd-list? item)
        (let* ((rx (cstr-funcall item startt false))
               (ry (cstr-argexprs (cdr args) (caddr rx))))
          (list
            (append (car rx) (car ry))
            (cons (cadr rx) (cadr ry))
            (caddr ry)
            (append (cadddr rx) (cadddr ry))
            (listmax (list (cadr (cdddr rx)) (cadr (cdddr ry))))))
        (let  ((ry (cstr-argexprs (cdr args) startt)))
          (list
            (car ry)
            (cons item (cadr ry))
            (caddr ry)
            (cons item (cadddr ry))
            (cadr (cdddr ry))))))))

(define (listmax l)
  (define (lm l z)
    (if (null? l) z (lm (cdr l) (if (> (car l) z) (car l) z))))
  (lm (cdr l) (car l)))

(define (mapn f l)
  (let loop ((ll l) (n 0) (rv '()))
    (if (null? ll) (reverse rv)
      (loop (cdr ll) (+ n 1) (cons (f (car ll) n) rv)))))

(define (cstr-funcall-primop flatx startt)
  (let* ((ae (cstr-argexprs (cdr flatx) (inc startt)))
         (tmps (tmpref-mk startt)))
    (list
      (append
        (apply append (map car ae))
        (list (list 'SET tmps (cons (primop-to-id x) ae))))
      tmps
      (listmax (cons startt (map caddr ae)))
      (apply append (map cadddr ae))
      (cadr (cdddr ae)))))

(define (cstr-funcall-local flatx startt istc)
  (let* ((ae (cstr-argexprs (cdr flatx) (if istc startt (inc startt))))
         (tmps (tmpref-mk startt)))
    (list
      (append
        (car ae)
        (if istc
          (list (list 'SET 'localsp (list 'LOCALSP_GET (car flatx) (length (cadr ae)) 'locals (list 'NOTP 'args 'jmpargs1 'jmpargs2))))
          (list))
        (mapn
          (lambda (x n)
            (list 'SET (if istc (string-append "localsp[" (number->string n) "]") (localref-mk n)) x))
          (cadr ae))
        (list
          (if istc
            (list 'TAIL_APPLY (car flatx) 'localsp (length (cadr ae)))
            (list 'SET tmps (list 'APPLY (car flatx) 'locals (length (cadr ae)))))))
      tmps
      (caddr ae)
      (cadddr ae)
      (listmax (list (cadr (cdddr ae)) (length (cadr ae)))))))

;; Currently we have a basic apply(fn, x)
;; but would like this to be apply(fn, xs, n)
(define (cstr-funcall x startt istc)
  (let* ((flatx (flatten-args x))
         (fn (car flatx)))
    (cond
      ((primop-symbol? fn) (cstr-funcall-primop flatx startt))
      ((or (varref? fn) (lamref? fn)) (cstr-funcall-local flatx startt istc))
      (else (print 'unsure-case)))))

(define (cstr-code x startt istc)
  (if (non-resvd-list? x)
    (cstr-funcall x startt istc)
    (list (list) x startt (list x) 0)))

;;;;;;;;;;;;;;;

(define (tocstr x inl)
  (cond
    ((symbol? x) (symbol->string x))
    ((varref? x) (string-append "VARIABLE_LOOKUP(" (number->string (varref-val x)) ")"))
    ((lamref? x)
      (if inl
        (string-append "closure_single(LAMBDA(" (number->string (lamref-id x)) "), VARIABLE_LOOKUP(0))")
        (string-append "LAMBDA(" (number->string (lamref-id x)) ")")))
    ((preval? x) (string-append "preval[" (number->string (preval-id x)) "]"))
    ((primop-symbol? x) (primop-to-id x))
    ((non-resvd-list? x) (cstr-funcall x 0 inl))))

(define (generate-externs)
  (apply string-append
    (map
      (lambda (p)
        (string-append "extern smooth_t " (car p)
          (if (= (cdr p) 0) ""
            (string-append " (" (implode ", " (n-of "smooth_t" (cdr p))) ")"))
          ";\n"))
      (table->list internal-primops))))

(define (list-contains? l x)
  (if (null? l)
    false
    (if (eq? (car l) x)
      true
      (list-contains? (cdr l) x))))

(define (accounting-varrefs l)
  (let loop ((rem l) (done '()) (rv '()))
    (if (null? rem)
      (reverse rv)
      (if (and (eq? (car (car rem)) 'INCREF) (varref? (cadr (car rem)))
            (not (list-contains? done (varref-val (cadr (car rem))))))
        (loop (cdr rem) (cons (varref-val (cadr (car rem))) done) rv)
        (loop (cdr rem) done (cons (car rem) rv))))))

(define (tail-call-optimise x tv)
  x)

(define (ref-refs arity refs)

  (define (extract-refs i remrefs)
    (if (null? remrefs)
      (cons 0 remrefs)
      (let ((c (extract-refs i (cdr remrefs)))
            (isref (and (varref? (car remrefs)) (= (varref-val (car remrefs)) i))))
        (cons (+ (if isref 1 0) (car c))
          (if isref (cdr c) (cons (car remrefs) (cdr c)))))))

  (let loop ((remrefs refs) (i 0) (s ""))
    (if (= i arity)
      (if (null? remrefs)
        s
        (loop (cdr remrefs) i  (string-append "      INCREF(" (itemstr (car remrefs) "") ");\n" s)))
      (let ((c (extract-refs i remrefs)))
      (loop
        (cdr c)
        (inc i)
        (if (= (car c) 0)
          (string-append "      DECREF(VARIABLE_LOOKUP(" (number->string i) "));\n" s)
          (if (= (car c) 1)
            s
            (string-append "      ADDREF(VARIABLE_LOOKUP(" (number->string i) "), " (number->string (dec (car c))) ");\n" s))))))))

(define (lam-or-clos-arity x)
  (+ (table-ref internal-arity-n x) (max 0 (table-ref internal-closure-n x))))

(define (remove-non-return-vars l x)
  (let ((rv (varref-val x))
        (larity (lam-or-clos-arity l)))
    (let loop ((i 0) (s ""))
      (if (= i larity)
        s
        (loop (inc i)
          (if (= i rv)
            s
            (string-append "      DECREF(VARIABLE_LOOKUP(" (number->string i) "));\n" s)))))))

(define (lambda-to-code x)
  (let* ((c  (cdr x))
         (tc (cstr-code c 0 true)))
    (list
      (caddr tc)
      (cadr (cdddr tc))
      (string-append "    case " (number->string (car x)) ":\n"
        (string-append
          (ref-refs (lam-or-clos-arity (car x)) (cadddr tc))
          (p-implode ""
            (map (lambda (x) (itemstr x "      ")) (tail-call-optimise (car tc) (cadr tc))))
          (if (tmpref? (cadr tc))
            ""
            "")
          (if (varref? (cadr tc))
            (string-append
              (remove-non-return-vars (car x) (cadr tc))
              "      PUSH("
              (itemstr (cadr tc) "")
              ");\n")
            ""))))))

(define (generate-lambdas-code)
  (let ((stuff (map lambda-to-code (reverse (table->list internal-lambdas)))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

(define (incref-initials l)
  (if (null? l)
    ""
    (string-append "  INCREF(" (itemstr (car l) "") ");\n" (incref-initials (cdr l)))))

(define (preval-to-code x)
  (let ((c (cstr-funcall (cdr x) 0 false)))
    (list
      (caddr c)
      (length (cdr (flatten-args (cdr x))))
      (string-append
        (incref-initials (cadddr c))
        (p-implode ""  (map (lambda (x) (itemstr x "  ")) (car c)))))))

(define (generate-prevals-code)
  (let ((stuff (map preval-to-code (reverse (table->list internal-prevals)))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

;; A bit hacky here, use reverse and hope that's the right order instead of sorting it...
(define (generate-arity-values)
  (implode ", " (map (lambda (x) (number->string (+ (cdr (car x)) (max (cdr (cdr x)) 0))))
                  (zip (reverse (table->list internal-arity-n)) (reverse (table->list internal-closure-n))))))

(define (generate-internal)
  (if (= internal-prevals-id 0)
    "
#include \"smoothlang/anc2020/smooth_core.h\"

int main (void) { return EXIT_SUCCESS; }

"
    (string-append "
#include \"smoothlang/anc2020/smooth_core.h\"

"
(generate-externs)
"

/*
 * Module optimisation definitions will allow static memory to be defined here
 * containing any kind of C data desired.
 */


/* Allocate some memory which we can shadow to use as ID addresses for our lambda tags. */
#define SMOOTH_LAMBDAS_LENGTH " (number->string internal-lambdas-id) "

smooth_size smooth_lambdas_length = SMOOTH_LAMBDAS_LENGTH;

static const smooth_size smooth_lambda_sizes[SMOOTH_LAMBDAS_LENGTH] = {
  " (generate-arity-values) "
};
"
(let ((lc (generate-lambdas-code)))
  (string-append
"
"
(if (zero? (cadr lc)) ""
  (string-append
"/*
 * One copy of variables that can be used to supply
 * args to a jmp call so args!=locals and therefore not trashed.
 */
static smooth jmpargs1[" (number->string (cadr lc)) "];
static smooth jmpargs2[" (number->string (cadr lc)) "];

static smooth* localsp;"
))
"

#ifdef SMOOTH_NATIVE_STACK
smooth smooth_execute (smooth pc, smooth self, smooth* args, smooth_size numlocals) {
#else
void smooth_execute (smooth pc) {
#endif

"
(if (zero? (cadr lc)) ""
  (string-append
"  smooth locals[" (number->string (cadr lc)) "];"
))
"
#ifdef SMOOTH_NATIVE_STACK
"
(if (zero? (car lc)) ""
  (string-append
"  smooth tmp[" (number->string (car lc)) "];"
))
"
#endif

jump:
  switch (pc) {
"
(implode "\n" (caddr lc))
"  }
"
))
"
  /* This will not happen but helps keep the compiler happy */
  return 0;
}

int main (int argc, char** argv) {

"
(let ((p (generate-prevals-code)))
  (string-append
"  smooth locals[" (number->string (cadr p)) "];
  smooth tmp[" (number->string (car p)) "];

  smooth_argc = argc;
  smooth_argv = argv;

  CORE_INIT(smooth_lambda_sizes);

"
  (implode "\n" (caddr p))
))
"  DECREF(tmp[0]);

  return EXIT_SUCCESS;
}

")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-output-to-c-port h p)
  ;; Since there is only one object in the file p-first selects this.
  (transform-internal (p-first p))
  (display (generate-internal) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse tree error checking
;;;
;;; This will need to check that all the required assumptions about the
;;; parse-tree being valid are in fact true.

(define (parse-check-errors h p) p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)

  (parse-output-to-c-port (current-output-port)

    (parse-phase-null

      (parse-strip-meta
        (parse-check-errors (current-error-port)
          (parse-input-from-port (current-input-port)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

