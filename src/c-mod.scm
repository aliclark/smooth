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

; The input to this stage takes up a very strict form.
;
; input is E
;
; where E can be
;   * ((()lambda) X E)
;   * (E E)
;   * E
;   * X  -- from an enclosing ((()lambda) X ...)
;   * (()primop ...)
;
; where X can be
;   * any non-internal symbol
;
; in (E1 E2), E1 can be a lambda for a recursive procedure.
;
; Any place a primop occurs, it must always be applied by at least the correct
; number of arguments.
; This is easily acheived at an earlier phase by wrapping them with lamdas of correct arity,
; eg. myprint -> (lambda (x y) (myprint x y))

; A couple of quick transformation we can do on the parse tree is to introduce
; lamref, varref and preval internal symbols, which are inserted into the tree
; in places where code has been taken out into a table.
;
; A preval is any primitive application that does not depend on free variables.
; A lambdid refers to a lambda expression.
; The lambda expression referred to by a lamref may refer to a free variable as a varref,
; and these reference to how far up the enclosure the variable was defined.
; eg. (varref 2) specifies that the variable is defined 2 closures above this lambda.
;
; The sanitiser should throw an error if a primop has arity 0 but is used
; as a function in a function application.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library code

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (every-true l) (if (null? l) true (and (car l) (every-true (cdr l)))))

(define (max-list l) (apply max l))

(define (n-of x n) (if (= n 0) nil (cons x (n-of x (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal library code

(define (varref-mk n) (reserved-symbol-mk 'varref (string->symbol (number->string n))))
(define (varref? x) (reserved-symbol-type? x 'varref))
(define (varref-val x) (string->number (symbol->string (reserved-symbol-val x))))
(define (varref-add x n) (varref-mk (+ (varref-val x) n)))

(define (lamref-mk n) (reserved-symbol-mk 'lamref (string->symbol (number->string n))))
(define (lamref? x) (reserved-symbol-type? x 'lamref))
(define (lamref-id x) (string->number (symbol->string (reserved-symbol-val x))))

(define (preval-mk n) (reserved-symbol-mk 'preval (string->symbol (number->string n))))
(define (preval? x) (reserved-symbol-type? x 'preval))
(define (preval-id x) (string->number (symbol->string (reserved-symbol-val x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define internal-prevals-id 0)
(define internal-lambdas-id 0)

(define internal-prevals   (make-table))
(define internal-lambdas   (make-table))
(define internal-closure-n (make-table))
(define internal-primops   (make-table))

(define (lamref-lookup x) (table-ref  internal-lambdas (lamref-id x)))
(define (lamref-set! x v) (table-set! internal-lambdas (lamref-id x) v))

;; closure-n values:
;; primop symbol: 0
;; variable:      1 for most recent lambda, 2 for next, etc.
;; list:          max closure-n value of elements
;; lambda:        closure-n value of body, minus one
;;
;; It also might be useful to cache the closure-n values for lists so it doesn't
;; need to be recalculated during copying.

(define (lamref-closure-n x) (table-ref internal-closure-n (lamref-id x)))
(define (lamref-is-closure? x) (> (lamref-closure-n x) 0))

(define (primops-list-add! p)
  (table-set! internal-primops (primop-to-id p) (primop-arity p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rearitise-leaves p)
  (p-list
    (if (lambda-expression? (p-first p))
      (lambda-expression-mk (lambda-expression-var (p-first p))
        (rearitise-primops (lambda-expression-body (p-first p))))
      (if (non-resvd-list? (p-first p))
        (p-list (rearitise-leaves (p-first (p-first p)))
          (rearitise-primops (p-second (p-first p))))
        (p-first p)))
    (rearitise-primops (p-second p))))

(define (deep-head-length- p n)
  (if (non-resvd-list? p)
    (deep-head-length- (p-first p) (inc n))
    (cons p n)))

(define (deep-head-length p) (deep-head-length- p 0))

(define (rearitise-flatten p)
  (p-append
    (if (non-resvd-list? (p-first p))
      (rearitise-flatten (p-first p))
      (p-list (p-first p)))
    (p-list (rearitise-primops (p-second p)))))

(define (rearitise-primop-arity p pn ln)
  (if (= pn ln)
    (rearitise-flatten p)
    (p-list (rearitise-primop-arity (p-first p) pn (dec ln)) (rearitise-primops (p-second p)))))

;; All function applications will be in the form of ((f x1) x2)
;; but this is unwieldy in the case of primops, for which we would like
;; (f x1 x2) so this does that.
;; Note that if eg. f is of arity 2, and is giving 4 args we get the following:
;; (((f x1 x2) x3) x4)
(define (rearitise-primops p)
  (cond
    ((lambda-expression? p)
      (lambda-expression-mk (lambda-expression-var p)
        (rearitise-primops (lambda-expression-body p))))
    ((non-resvd-list? p)
      (let ((h (deep-head-length p)))
        (if (primop-symbol? (car h))
          (begin
            (primops-list-add! (car h))
            (rearitise-primop-arity p (primop-arity (car h)) (cdr h)))
          (rearitise-leaves p))))
    (else p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (depth-align-varn expression vname n)
  (cond
    ((lambda-expression? expression)
      (lambda-expression-mk (lambda-expression-var expression)
        (depth-align-varn
          (depth-align-var (lambda-expression-body expression) (lambda-expression-var expression))
          vname
          (+ n 1))))
    ((non-resvd-list? expression) (map (lambda (x) (depth-align-varn x vname n)) expression))
    ((eq? expression vname) (varref-mk n))

    ;; Else expression was some reserved symbol
    (else expression)))

(define (depth-align-var expression vname) (depth-align-varn expression vname 0))

;; This will zip through all the code elements, replacing variables
;; with references to the level at which they are defined,
;; which is possible since only one variable is defined at a time by any lambda.
(define (replace-varrefs expression)
  (cond
    ((lambda-expression? expression)
      (lambda-expression-mk (lambda-expression-var expression)
        (depth-align-var (lambda-expression-body expression)
          (lambda-expression-var expression))))
    ((non-resvd-list? expression) (p-map replace-varrefs expression))
    (else expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (lambda a (lambda b a))
;; (lambda a (lambda b b))

(define (send-out-lambda p)
  (let ((rv (send-out-lambdas- (lambda-expression-body p))))
    (table-set! internal-lambdas   internal-lambdas-id (car rv))
    (table-set! internal-closure-n internal-lambdas-id (dec (cdr rv)))

    (set! internal-lambdas-id (+ internal-lambdas-id 1))
    (cons (lamref-mk (- internal-lambdas-id 1)) (dec (cdr rv)))))

;; (lambda x (do_shizzle1 (lambda y (do_shizzle2 global_c_var))))
;;
;; lambda[0] = (do_shizzle2 global_c_var)
;; lambda[1] = (do_shizzle1 lambda[0])
;; lambda[1]

(define (send-out-lambdas- p)
  (cond
    ((lambda-expression? p)
      (let ((lamdid (send-out-lambda p)))
        (cons (car lamdid) (cdr lamdid))))
    ((non-resvd-list? p)
      (let ((l (p-map send-out-lambdas- p)))
        (cons (p-map car l) (max-list (p-map cdr l)))))
    ((varref? p) (cons p (inc (varref-val p))))
    (else (cons p 0))))

(define (send-out-lambdas p) (car (send-out-lambdas- p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define garbage-lambdas p-null)

;; This should copy any lambdas and update reference numbers on varrefs.
;; Obviously, this is a highly expensive operation,
;; so I would like to find a completely different algorithm for reduction if possible.
;;
;; However, since refs may need to be updated anyway, it is probably necessary
;; to walk at least some of the code.
;;
;; This also needs to propogate info on the new closure-n values for bits of code.
(define (update-refs val depth)
  (cond
    ((lamref? val) val)
    ((non-resvd-list? val) (p-map (lambda (x) (update-refs x depth)) val))
    ((varref? val) (varref-add val depth))
    (else val)))

(define (apv-mk ex replp) (list ex replp))
(define apv-ex    car)
(define apv-replp cadr)

;; We know that we need only look inside closures and not lambdas,
;; since only closures can reference the variable we are interested in.
;; Not only that, but for any depth D from the start, we are only interested
;; in searching lambdas with closure value (> D) where D is 0 initially.
;;
;; Note:
;; This copies val wherever a varref is found that refers to it.
;; This is not correct, because it means all the copies will refer to the same
;; bit of mutable code. Some sort of deep copy is needed.
;;
;; Also note that when being copied in, any varrefs in the val need to be
;; corrected to point the right number of frames up the stack.
;; That's not too bad, we only need to correct inside closures, and correcting
;; is basically adding curdepth to the varref value.
;;
;; Additionally, the closure-n value of any lambdas containing the varref
;; will be changed, which although not hideously expensive, is still quite annoying.
;;
(define (apply-value c val curdepth)
  (cond
    ((lamref? c)
      (if (> (lamref-closure-n c) curdepth)
        (begin
          (lamref-set! c (apply-value (lamref-lookup c) val (inc curdepth)))
          c)
        c))
    ((non-resvd-list? c) (p-map (lambda (x) (apply-value x val curdepth)) c))
    ((varref? c) (if (= (varref-val c) curdepth) (update-refs val curdepth) c))
    (else c)))

(define (rex-mk ex contp) (list ex contp))
(define rex-ex    car)
(define rex-contp cadr)

(define (reduce-expression- p)
  (cond

    ((lamref? p)
      ;; Expression is lambda, so reduce it's insides
      (let ((s (reduce-expression- (lamref-lookup p))))
        (lamref-set! p (rex-ex s))
        (rex-mk p (rex-contp s))))

    ((non-resvd-list? p)

      ;; Expression is an application, so do the application
      (cond

        ((lamref? (p-first p))
          (set! garbage-lambdas (cons (p-first p) garbage-lambdas))
          (reduce-expression- (apply-value (lamref-lookup (p-first p)) (p-second p) 0)))

        ((non-resvd-list? (p-first p))
          ;; First reduce the head, then if we are okay, continue as normal.
          (let* ((h  (reduce-expression- (p-first p)))
                 (hx (rex-ex h)))

            ;; rex-contp tells us if we can continue down the line or not
            (if (rex-contp h)

              (cond
                ((lamref? hx)
                  (set! garbage-lambdas (cons hx garbage-lambdas))
                  (reduce-expression- (apply-value (lamref-lookup hx) (p-second p) 0)))
                (else
                  (let ((s (reduce-expression- (p-second p))))
                    (rex-mk (p-list hx (rex-ex s)) (rex-contp s)))))

              (rex-mk (p-list hx (p-second p)) false))))

        (else (rex-mk p true))))

    (else (rex-mk p true))))

(define (reduce-expression p) (rex-ex (reduce-expression p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ((a b c) d)   -- current evaluation order
;
; For any given lambda, there may be zero of more prevals, which are
; bits of code that can execute immediately.
; If a preval is contained by a preval, then only the outer preval need be taken out,
; since it contains the inner preval anyway.
; However, currently, the compiler will just take out any prevals even if their parent
; is also a preval.

;; The only reason to not be eval ready is if we reference a local variable on stack.

(define (send-out-preval p)
  (table-set! internal-prevals internal-prevals-id p)
  (set! internal-prevals-id (+ internal-prevals-id 1))
  (cons (preval-mk (- internal-prevals-id 1)) true))

;; This will find the largest preval sections of code and place them in the
;; internal-prevals structure, leaving a reference symbol in it's place.
;;
;; lambda[0] = (do_shizzle2 global_c_var)
;; lambda[1] = (do_shizzle1 lambda[0])
;; lambda[1]
;;
;; In this case, do_shizzle2 will be the first in order to be executed.
;; The following structure ensues:
;;
;; preval[0] = (do_shizzle2 global_c_var)
;; preval[1] = (do_shizzle1 lambda[0])
;; lambda[0] = preval[0]
;; lambda[1] = preval[1]
;; lambda[1]
;;
;; Starting with the root node we first go into the code, sending out prevals
;; and the return value will be true if the child was pre-executable.
;;
;; Returns a pair of (expression . evalready?)
;;
;; The depth variable allows us to know whether a lambda in the code is referring
;; to any variables above our own level. If not, it is eval ready.

;; It would help if we had a quick way to signal whether a given lambda was closed over or not.

(define (send-out-prevals- p)
  (cond
    ((lamref? p)
      (let ((s (send-out-prevals- (lamref-lookup p))))
        (lamref-set! p (car s))
        (cons p (not (lamref-is-closure? p)))))
    ((varref? p) (cons p false))
    ((non-resvd-list? p)
      (let ((l (map send-out-prevals- p)))
        (if (every-true (map cdr l))
          (send-out-preval (map car l))
          (cons (map car l) false))))
    (else (cons p true))))

(define (send-out-prevals p) (car (send-out-prevals- p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This will take a normal parse tree and pull bits of it out into hash tables,
;; leaving references in their place.

(define (transform-internal p)
  (send-out-prevals (reduce-expression (send-out-lambdas (replace-varrefs (rearitise-primops p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tocstr x inl)
  (cond
    ((symbol? x) (symbol->string x))
    ((varref? x) (string-append "VARIABLE_LOOKUP(" (number->string (varref-val x)) ")"))
    ((lamref? x)
      (if inl
        (string-append "CLOSURE(" (number->string (lamref-id x)) ")")
        (string-append "LAMBDA(" (number->string (lamref-id x)) ")")))
    ((preval? x) (string-append "preval[" (number->string (preval-id x)) "]"))
    ((primop-symbol? x) (primop-to-id x))
    ((non-resvd-list? x)
      (if (primop-symbol? (p-first x))
        (string-append (tocstr (p-first x) inl) "("
          (implode ", " (map (lambda (p) (tocstr p inl)) (tail x)))
          ")")
        (string-append "APPLY(" (tocstr (p-first x) inl) ", "
          (tocstr (p-second x) inl)
          ")")))))

(define (generate-externs)
  (apply string-append
    (map
      (lambda (p)
        (string-append "extern smooth_t " (car p)
          (if (= (cdr p) 0) ""
            (string-append " (" (implode ", " (n-of "smooth_t" (cdr p))) ")"))
          ";\n"))
      (table->list internal-primops))))

(define (lambda-to-code x)
  (string-append "    case " (number->string (car x)) ":\n"
  (let ((c (cdr x)))
    (string-append "      PUSH(" (tocstr c true) ")\n"))
  "      break;\n"))

(define (generate-lambdas-code)
  (implode "\n" (map lambda-to-code (table->list internal-lambdas))))

(define (preval-to-code x)
  (string-append "  preval[" (number->string (car x)) "] = "
    (tocstr (cdr x) false)
    ";\n"))

(define (generate-prevals-code)
  (implode "\n" (map preval-to-code (reverse (table->list internal-prevals)))))

(define (generate-internal)
  (if (= internal-prevals-id 0)
    "
#define EXIT_SUCCESS 0

int main (void) { return EXIT_SUCCESS; }

"
    (string-append "
#include \"smoothlang/anc2020/smooth_core.h\"

"
(generate-externs)
"
static smooth_t preval[" (number->string internal-prevals-id) "];

/* Allocate some memory which we can shadow to use as ID addresses for our lambda tags. */
#define SMOOTH_LAMBDAS_LENGTH " (number->string internal-lambdas-id) "
unsigned long int smooth_lambdas_length = SMOOTH_LAMBDAS_LENGTH;
byte smooth_lambdas_start[SMOOTH_LAMBDAS_LENGTH];

#if 0

/* Not sure if it is good to change to this definition, but something worth considering. */
void smooth_execute (smooth_t pc, smooth_closure_t* self, smooth_t local) {
  unsigned long int i;

#else

void smooth_execute (smooth_t pc) {
  unsigned long int i;
  smooth_closure_t* self;
  smooth_t local;

#endif

#if 0
jump:
#endif

  switch (pc) {
"
(generate-lambdas-code)
"  }

}

int main (const int argc, const char** const argv) {

  smooth_argc = (smooth_t) argc;
  smooth_argv = (smooth_t) argv;

  CORE_INIT();

"
(generate-prevals-code)
"
  return EXIT_SUCCESS;
}

")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-output-to-c-file h p)
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

  (parse-output-to-c-file (current-output-port)

    (parse-phase-null

      (parse-strip-meta
        (parse-check-errors (current-error-port)
          (parse-input-from-file (current-input-port)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

(start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

