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


;
; Note: currently the ref counting is not correct for the case
; where varref is used as the function.
;




;Current problem:
;
; There are two main bottlenecks for multithreading.
;
; The first is that currently threads share access to the closures memory space.
;  This space is needed to be well defined for type checking on function calls.
;
; The second is the copy of call mechanism used by closures to allow a closure to be called multiple times.
;  This likewise has to be locked to ensure multiple threads don't compete over whether copy_on_call is set.
;  Note that this problem case will completely disappear if we go back to using one var per lambda
;  or use copy on call all of the time (as opposed to only after the initial call).
;  Thus it will not be considered for now.
;
; The third case likewise the first but with the garbage collection system
;
; The other cases could be solved if we had the closures mem and/or the garbage collection
; a one per thread.
;
; This could mean that we continually pass around a thread id (or a thread data structure)
; between functions so we know which of the global thread models we are currently referring to.
;
; This would make our native calling system somewhat uglier :(
;
; That is only half the story;
; we still need to figure out how to create the memory for a new thread
; and also what happens when someone wants to use data from one thread over in another one.
;
; It might be helpful to find out how Erlang solves these issues.
;
; *make sure malloc/realloc/free is thread safe else make some wrappers for the allocations.
;
; Because of unboxedness, it is unknown at any given time whether a value
; is recorded in a given thread's system, or just coincidentally has the same number.
;
; It is equally unknown whether an argument value is a pointer to a closure or just a corresponding number.
;
; Currently we have:
; smooth_call(f, x)
;
; This would have to change to:
; smooth_call(thread, f, x)
;
; Because since f is an int_ptr just like any other, we would not know which thread's closures it could correspond to.
; Attempting to check each thread's closure list whenever we make a function call is not an option!
;
; With this restriction in place we need to know whether it is really viable to restrict calls in this way to a per thread basis.
;

; EVALUATION STRATEGY:
;
; The default execution model at run-time will be entirely call by need
; (whereas at compile time all lambdas are fully reduced, until a cycle occurs).
;
; However, the compiler will accept hints from module functions to say
; whether the arguments are strict or lazy.
;
; This may mean that the following definition:
;
;   smooth_t smoothlang_anc2020_cbool__if (smooth_t test, smooth_t a, smooth_t b);
;
; can be changed to the following:
;
;   smooth_t smoothlang_anc2020_cbool__if (smooth_t test, lazy_t a, lazy_t b);
;
; likely implementation:
;
;   smooth_t smoothlang_anc2020_cbool__if (smooth_t test, lazy_t a, lazy_t b) {
;     return test ? a : b;
;   }
;
; The c-mod phase will notice this declaration and will find the value of test
; before calling cbool__if() but will thunk up 'a' and 'b'.
; The user will then be responsible for calling smooth_force on the return value
; to ensure it gets evaluated.
;
; smooth_t smooth_force (smooth_t x); // does exactly what it says on the tin
;
; Also allow lazy hints to be specified using some other method not directly
; affecting the C code such as a special comment like:
;
; /*SMOOTH smooth_t smoothlang_anc2020_cbool__if (smooth_t test, lazy_t a, lazy_t b); */
;
; This approach to evaluation removes possibilty for the warm and fuzzy definition of `if`,
; that is, a simple function that returns a lazily executed value.
;
; It does however maintain the possibility of running the entire vm unboxed,
; and this has always been a core priority of smooth.
;
; This approach will be better suited than another one for the majority of users.
;


;
; Instead of repeating the same optimisation code over and over again,
; it would be nice to be able to run module code directly from the compiler,
; so eg.
;   SMOOTH_OPT smooth_t square (smooth_t x) { return x * x; }
; could be called when a value is available, with the result being used instead.
;
; It might not be possible to use this in all places when considering pointers
; as opposed to raw numerical values, and it would probably be necessary
; to keep use of additional ways to specify optimisations.
;


;
; Something to look out for:
; If a lambda is applied twice in the same C module call, eg:
;
; r1 = smooth_apply(f1, x1);
; r2 = smooth_apply(f1, x2);
;
; then if the call is adding a variable to a closure,
; the closure must either be copied for at least the second call,
; or it must be created dynamically for at least the second call.
;
; We add the copy_on_write flag.
; Upon completion of the first call, this is set true.
; For all successive calls on the closure object, a copy is made.
;
; Of course, copy_on_write will thus need protection for multithreading;
; This is a major headache, unless we're okay with creating thousands of mutexes.
;
;
;
; typedef struct closure {
;   closure* parent;
;   smooth_t code;
;   smooth_t local; /* Either a pointer to locals or a single local */
;   /* The following were added to the model to allow O(1) varrefs */
;   unsigned int num_locals; /* if num_locals=1 `local` is a value, else ptr */
;   unsigned int locals_depth; /* how many arguments have been filled */
;   bool copy_on_write; /* tells us whether the closure needs copying next call */
; } closure;
;


;
; typedef struct lazy {
;   bool unrun; // this needs better locking if we want to ensure running once only.
;   smooth_t preval[3]; // however many prevals we have
;   void (*func)(struct lazy*);
;   closure* wrt; // with respect to a closure (possibly null).
; } lazy;
;
; void internal_lazy_0 (lazy* self) {
;   if (unrun) {
;     unrun = false;
;     self->preval[0] = smoothlang_anc2020_putchar('b', LAMBDA(32));
;   }
; }
;
; The main program expression will be interpreted as a lazy expression,
; and as will use the canonical smooth_force(lazyval) for elegance
; even if it is slightly slower than hardcoding in the correct bits of code.
;
; * Every expression that is used more than once MUST be stored after its first calculation
;   and the stored value used from then onwards.
;   It is still undecided on which object the preval is to be stored,
;   but it is preferably as close to the object that binds the last value as possible.
; * Each expression must be calculated as soon as enough variable values are known,
;   and the closest enclosing lazy section has been forced.
;   (for a putchar('b') type expression,
;    this will be immediately upon forcing the main expression,
;    since all arguments are known at that point).
;
; A strict value can be modelled as a lazy value that has been smooth_forced immediately.
; it is only through optimisations that we are able to remove the thunking and forcing,
; but semantically they are there, albeit pointlessly.
;


;
; A moderate incompatibily between smooth programs and plain C code:
; * floats and doubles, while not impossible to create via division of smooth_t integers,
;   are quite expensive to get into programs initially (but not too tricky once created).
;
; Since we are only writing out a single C file anyway,
; these float constants will have to be written out to memory there to a static array.
;
; Within the program, calls such as smooth_float_create(1, 3) /* representing (1/3) */
; can easily be replaced by a pointer to the float value in our initial array.
;
; /* somewhere in the variable definitions */
; static double smoothlang_anc2020_double__initdata[3] = { 3.23452, 435.56236, 0.0320234 };
;
; /* somewhere in the program */
; smoothlang_anc2020_double__divide(smoothlang_anc2020_double__initdata + 2,
;                                   smoothlang_anc2020_double__initdata + 0);

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

(define (tmpref-mk n) (reserved-symbol-mk 'tmpref (string->symbol (number->string n))))
(define (tmpref? x) (reserved-symbol-type? x 'tmpref))
(define (tmpref-id x) (string->number (symbol->string (reserved-symbol-val x))))

(define (localref-mk n) (reserved-symbol-mk 'localref (string->symbol (number->string n))))
(define (localref? x) (reserved-symbol-type? x 'localref))
(define (localref-id x) (string->number (symbol->string (reserved-symbol-val x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define internal-prevals-id 0)
(define internal-lambdas-id 0)

(define internal-prevals   (make-table))
(define internal-lambdas   (make-table))
(define internal-closure-n (make-table))
(define internal-arity-n   (make-table))
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
  (if (lambda-expression? p)
    (lambda-expression-mk (lambda-expression-var p)
      (rearitise-primops (lambda-expression-body p)))
    (p-list
      (if (lambda-expression? (p-first p))
        (lambda-expression-mk (lambda-expression-var (p-first p))
          (rearitise-primops (lambda-expression-body (p-first p))))
        (if (non-resvd-list? (p-first p))
          (p-list (rearitise-leaves (p-first (p-first p)))
            (rearitise-primops (p-second (p-first p))))
        (p-first p)))
      (rearitise-primops (p-second p)))))

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
;  (pretty-print p)
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

    ;; We need to do checking here to see if rv is a lambdid
    ;; if it is then we want to take return its lambdid, but w
    (if (lamref? (car rv))

      (let ((lid (lamref-id (car rv))))
        (table-set! internal-closure-n lid (dec (cadr rv)))
        (table-set! internal-arity-n   lid (inc (caddr rv)))
        (list (car rv) (dec (cadr rv)) (inc (caddr rv ))))

      (begin

        (table-set! internal-lambdas   internal-lambdas-id (car rv))
        (table-set! internal-closure-n internal-lambdas-id (dec (cadr rv)))
        (table-set! internal-arity-n   internal-lambdas-id (inc (caddr rv)))

        (set! internal-lambdas-id (+ internal-lambdas-id 1))

        (list (lamref-mk (- internal-lambdas-id 1)) (dec (cadr rv)) (inc (caddr rv )))))))

;; (lambda x (do_shizzle1 (lambda y (do_shizzle2 global_c_var))))
;;
;; lambda[0] = (do_shizzle2 global_c_var)
;; lambda[1] = (do_shizzle1 lambda[0])
;; lambda[1]

(define (send-out-lambdas- p)
  (cond
    ((lambda-expression? p)
      (let ((lamdid (send-out-lambda p)))
        (list (car lamdid) (cadr lamdid) (caddr lamdid))))
    ((non-resvd-list? p)
      (let ((l (p-map send-out-lambdas- p)))
        (list (p-map car l) (max-list (p-map cadr l)) 0)))
    ((varref? p) (list p (inc (varref-val p)) 0))
    (else (list p 0 0))))

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

(define (reduce-expression p) (rex-ex (reduce-expression- p)))

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


;; Turn off smooth compiler optimisations

(define (reduce-expression x) x)

(define (send-out-prevals  x)
  (send-out-preval x))


(define (transform-internal p)
  (send-out-prevals (reduce-expression (send-out-lambdas (replace-varrefs (rearitise-primops p))))))

; Possible alternative stage order
;
; (send-out-prevals [ (group-closures (send-out-lambdas  (rearitise-primops ] (replace-varrefs (reduce-expression))))))
;
; Here the brace is used to indicate passes that could potentially be merged into a single pass.
;
; It may even be possible to merge "send-out-prevals" in just before "group-closures",
; but I'll wait until the first 3 here are merged successfully before attempting.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;The anatomry of function code generation...
;
;There is not much complicated stuff that needs to be done really.
;
;A lambda body has either a variable lookup or a function call.
;
;The function call may make use of 'tmp's and it may make use of prevals in future
;
;The code will incref everything before it is called or called with initially,
;but will not do incref's on subsequent intermediate values
;(unless it is a local variable being used in more than one place)



(define (flatten-args x)
  (if (and (non-resvd-list? x) (non-resvd-list? (car x)))
    (cons (flatten-args (caar x)) (cons (cadar x) (cdr x)))
    x))

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

(define (itemstr- x inl id suppress)
  (cond
    ((symbol? x) (symbol->string x))
    ((number? x) (number->string x))
    ((tmpref? x) (string-append "tmp[" (number->string (tmpref-id x)) "]"))
    ((varref? x) (string-append "VARIABLE_LOOKUP(" (number->string (varref-val x)) ")"))
    ((localref? x) (string-append "locals[" (number->string (localref-id x)) "]"))
    ((lamref? x)
      (if inl
        (string-append "closure_single(LAMBDA(" (number->string (lamref-id x)) "), VARIABLE_LOOKUP(0))")
        (string-append "LAMBDA(" (number->string (lamref-id x)) ")")))
    ((preval? x) (string-append "preval[" (number->string (preval-id x)) "]"))
    ((primop-symbol? x) (primop-to-id x))
    ((non-resvd-list? x)
      (string-append id (itemstr- (car x) inl id true) "(" (implode ", " (map (lambda (y) (itemstr- y inl "" true)) (cdr x)))
        (if suppress ")" ");\n")))
    (else (pretty-print x))))

(define (itemstr x inl id) (itemstr- x inl id false))

(define (cstr-argexprs args startt)
  (if (null? args)
    (list (list) (list) startt (list) startt)
    (let ((item (car args)))
      (if (non-resvd-list? item)
        (let* ((rx (cstr-funcall item startt))
               (ry (cstr-argexprs (cdr args) (caddr rx))))
          (list
            (append (car rx) (car ry))
            (cons (cadr rx) (cadr ry))
            (caddr ry)
            (append (cadddr rx) (cadddr ry))
            (listmax (list (cadr (cdddr rx)) (cadr (cdddr ry))))))
        (let  ((ry (cstr-argexprs (cdr args) startt)))
          (list
            (cons (list 'INCREF item) (car ry))
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

(define (cstr-funcall-local flatx startt)
  (let* ((ae (cstr-argexprs (cdr flatx) (inc startt)))
         (tmps (tmpref-mk startt))
         (setl (list 'SET tmps (list 'APPLY (car flatx) 'locals (length (cadr ae))))))
    (list
      (append
        (car ae)
        (list (list 'INCREF (car flatx)))
        (mapn (lambda (x n) (list 'SET (localref-mk n) x)) (cadr ae))
        (list setl))
      tmps
      (caddr ae)
      (cadddr ae)
      (listmax (list (cadr (cdddr ae)) (length (cadr ae)))))))

;; Currently we have a basic apply(fn, x)
;; but would like this to be apply(fn, xs, n)
(define (cstr-funcall x startt)
  (let* ((flatx (flatten-args x))
         (fn (car flatx)))
    (cond
      ((primop-symbol? fn) (cstr-funcall-primop flatx startt))
      ((or (varref? fn) (lamref? fn)) (cstr-funcall-local flatx startt))
      (else (print 'unsure-case)))))

(define (cstr-code x startt)
  (if (non-resvd-list? x)
    (cstr-funcall x startt)
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

(define (lambda-to-code x)
  (let* ((c  (cdr x))
         (tc (cstr-code c 0)))
    (list
      (caddr tc)
      (cadr (cdddr tc))
      (string-append "    case " (number->string (car x)) ":\n"
          (string-append
            (p-implode ""
              (map (lambda (x) (itemstr x true "      ")) (accounting-varrefs (car tc))))
            (if (tmpref? (cadr tc))
              "      SCOPE_REMOVE();\n"
              "")
            "      PUSH("
            (if (varref? (cadr tc))
              (string-append "SCOPE_DOWN(" (itemstr (cadr tc) true "") ")")
              (itemstr (cadr tc) true ""))
            ");\n")))))

(define (generate-lambdas-code)
  (let ((stuff (map lambda-to-code (reverse (table->list internal-lambdas)))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

(define (preval-to-code x)
  (let ((c (cstr-funcall (cdr x) 0)))
    (list
      (caddr c)
      (length (cdr (flatten-args (cdr x))))
      (p-implode ""  (map (lambda (x) (itemstr x false "  ")) (car c))))))

(define (generate-prevals-code)
  (let ((stuff (map preval-to-code (reverse (table->list internal-prevals)))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

;; A bit hacky here, use reverse and hope that's the right order instead of sorting it...
(define (generate-arity-values)
  (implode ", " (map (lambda (x) (number->string (cdr x))) (reverse (table->list internal-arity-n)))))

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


#ifdef NATIVE_STACK
smooth smooth_execute (smooth pc, smooth self, smooth* args, smooth_size numlocals) {
#else
void smooth_execute (smooth pc) {
#endif
"
(let ((lc (generate-lambdas-code)))
  (string-append
"
"
(if (zero? (cadr lc)) ""
  (string-append
"  smooth locals[" (number->string (cadr lc)) "];"
))
"
#ifdef NATIVE_STACK
"
(if (zero? (car lc)) ""
  (string-append
"  smooth tmp[" (number->string (car lc)) "];"
))
"
  int i;
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

