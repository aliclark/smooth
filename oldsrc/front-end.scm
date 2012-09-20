
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


; Ref counting can lead to circular reference even in closures.
; However, it should be noted that only the fill-args nature of the closure
; is what allows circular reference
;
; | -> a
; | -> a  -> b
; | -> a <-> b
; |    a <-> b
;
; This could cause a real problem if the closure also references large objects.
;
; Therefore it is very wise to instead look at changing the GC system,
; as the user should really not have to alter the way they program or be on the look out for this,
; as it is a perfectly valid way to write programs
;
; Circular link detection would also be a viable solution,
; but would need to have the type information to know whether a given object is actually a closure
; or simply a number that looks like a closure pointer.
;
; Here we could also adopt a conservative approach and assume all such numbers are actually closure pointers
; but this is not viable
;



; EVALUATION STRATEGY:
;
; The default execution model at run-time will be entirely call by need
; (whereas at compile time all lambdas are fully reduced, until a cycle occurs).
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
; A moderate incompatibily between smooth programs and plain C code:
; * floats and doubles, while not impossible to create via division of smooth_t integers,
;   are quite expensive to get into programs initially (but not too tricky once created).
;
; Since we are only writing out a single C file anyway,
; these float constants will have to be written out to memory there to a static array.
;
; Within the program, calls such as smooth_float_divide(1, 3) /* representing (1/3) */
; can easily be replaced by a pointer to the float value in our initial array.
;
; /* somewhere in the variable definitions */
; static double smoothlang_anc2020_double__initdata[3] = { 3.23452, 435.56236, 0.0320234 };
;
; /* somewhere in the program */
; smoothlang_anc2020_double__divide(smoothlang_anc2020_double__initdata + 2,
;                                   smoothlang_anc2020_double__initdata + 0);

;
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
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library code

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (every-true l) (if (null? l) true (and (car l) (every-true (cdr l)))))

(define (max-list l) (apply max l))

(define (n-of x n) (if (zero? n) nil (cons x (n-of x (dec n)))))

(define (zip a b)
  (if (null? a)
    p-null
    (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (max a b) (if (> a b) a b))

(define (fflip f) (lambda (a) (lambda (b) (f b a))))

(define (fcompose f g) (lambda (x) (f (g x))))

(define number->symbol (fcompose string->symbol number->string))
(define symbol->number (fcompose string->number symbol->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal library code

(define (numbered-symbol-maker s)
  (lambda (n)
    (reserved-symbol-mk s (number->symbol n))))

(define numbered-symbol-type-checker (fflip reserved-symbol-type?))

(define numbered-symbol-value-get (fcompose symbol->number reserved-symbol-val))

(define-macro (numbered-symbol-create! s . accs)
  (define (symbol-append . args)
    (string->symbol (apply string-append (map symbol->string args))))

  `(begin
    (define ,(symbol-append s '-mk)  (numbered-symbol-maker        ',s))
    (define ,(symbol-append s '?)    (numbered-symbol-type-checker ',s))
    ,@(let loop ((raccs accs))
      (if (null? raccs)
        '()
        (cons
          `(define ,(symbol-append s '- (car raccs)) numbered-symbol-value-get)
          (loop (cdr raccs)))))))

(numbered-symbol-create! varref   val)
(define (varref-add x n) (varref-mk (+ (varref-val x) n)))

(numbered-symbol-create! lamref   id)
(numbered-symbol-create! preval   id)
(numbered-symbol-create! tmpref   id)
(numbered-symbol-create! localref id)
(numbered-symbol-create! marker   id)


;; what we really need is two separate domains,
;; one of Scheme types, and one of Smooth types,
;; with easy conversion between the two.

;(define-type-info! lamref '(id number))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-lam          -1)

(define internal-lambdas-id 1)
(define internal-markers-id 0)

(define internal-lambdas    (make-table))
(define internal-closure-n  (make-table))
(define internal-closure-of (make-table))
(define internal-arity-n    (make-table))
(define internal-primops    (make-table))

; lambda -> [ lambda, ... ]
(define internal-closures-mine (make-table))

; id -> code-item
(define internal-markers    (make-table))

;; array of lambda -> [ marker, ... ]
(define internal-marker-known-at (make-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define internal-prevals-id 0)
(define internal-prevals    (make-table))


;; This is a preval-mk, with value indirected into a table.
;; The table allows the value to be easily looked up from elsewhere.
(define (internal-preval-mk p)
  (table-set! internal-prevals internal-prevals-id p)
  (set! internal-prevals-id (+ internal-prevals-id 1))
  (preval-mk (- internal-prevals-id 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; TODO: finish closure variable access propogation

;; (lambda a (lambda b a))
;; (lambda a (lambda b b))

;
; Things that can be preval'ed are any application of a function to one argument or more.
;
; In cases of primitive calls, this might be more, but otherwise it is just one.
;
; It is always true that the application will either have an argument var(0)
; or that the function itself is the var(0) - unless there are no vars used.
;; >Really?


;(lam f (a b (lamb x (f x)) c))

;; An expression keeps propogating up the lambda chain until part of it needs the variable defined at that level.
;; At that point, any expression it is embedded in will also stop, and so on,
;; but other children of those expressions will continue propogating.



;
; In the case where the child is a lambda,
; we need to go in to it's closure variables list and mutate it
; to decrease the closure-n count of each and remove any now at var(0)
;
; In the case where the child is anything else,
; we need to create a new closure variables list entry for them (after decrementing).
;
; * If it is a list, search the list for any lambdas.
;     These lambdas have their parent value set to ur lambda id.
;


; once we have the tree containing all lambda paths with links in both directions,
; we then have all the info we'll need to be able to reference closure vars later on.

; we cannot do that in this pass however, since we need to fully propogate to the top
; before we know in which scope are closure vars will have been bound to heap mem.

; it would be possible to avoid a separate pass however,
; if we pass downward the scope chain format in a linked list, eg. 2 -> 1 -> 3 -> 1*


;
; If we only need a closure variable for something that is used in a preval,
; and that preval is unconditionally evaluated,
; then we should not store the closure variable (and this could help remove a scope chain).
;
; (lambda x (cfoo (* x x) (lambda y (* x x))))
;
; Here closure-y can very happily only point to the preval mem for "(* x x)", not 'x' itself.
;


;
; For each closure we keep track of three bits of info:
;
; * closure variables (any vars defined outside)
; * unconditional prevals (preval is executed as soon as it can)
; * conditional prevals (preval is executed only when needed)
;
; There is difficulty because the initial list of closure variables
; can be further reduced in some cases with unconditional prevals
;


(define (child-lambdas l)
  (cond
    ((lamref? l) (list l))
    ((non-resvd-list? l) (apply append (p-map child-lambdas l)))
    (else (list))))


;; We are currently working on adding markers, see the list traversing part.


;;
;; Same argumennts as send-out-lambdas- except that we know that
;; 'p' is a lambda. We save relevant info on it and return an identifier for it.
;;
(define (send-out-lambda p parents)

  (define c (lambda-expression-body p))
  (define n 1)

  ;; Group any immediate lambdas into this one
  (let loop ()
    (if (lambda-expression? c)
      (begin
        (set! c (lambda-expression-body c))
        (set! n (inc n))
        (loop))
      #f))

  (let ((ili internal-lambdas-id))
    (set! internal-lambdas-id (+ internal-lambdas-id 1))

    (let ((rv (send-out-lambdas- c (cons (cons ili n) parents))))

      ;; TODO
      ;; before creating a new lambda,
      ;; it is very possible that a lambda already exists with the same code
      ;; and we can just use that.
      ;; It only makes sense to do this for plain lambdas (not closures)
      ;; except in the highly unlikely event that two closures
      ;; access all of their free vars at the same scope levels

      ;; update the relationships of who is parent of whom
      ;; todo: maintain this info as we traverse

      (let ((cls (child-lambdas rv)))
        (map (lambda (x) (table-set internal-closure-of x ili)) cls)
        (table-set! internal-closures-mine ili cls))

      (table-set! internal-lambdas   ili (car rv))
      (table-set! internal-closure-n ili (- (cadr rv)  n))
      (table-set! internal-arity-n   ili (+ (caddr rv) n))

      (list (lamref-mk ili) (- (cadr rv) n) (+ (caddr rv) n) (cadddr rv)))))

;; (lambda x (do_shizzle1 (lambda y (do_shizzle2 global_c_var))))
;;
;; lambda[0] = (do_shizzle2 global_c_var)
;; lambda[1] = (do_shizzle1 lambda[0])
;; lambda[1]

;;
;; p:       A Smooth Expression
;; parents: Linked list of items (ili, n)
;;          where 'ili' is a lambda id, and 'n' its arity
;;
;; rv: Linked list holding at each index:
;;     0: the resulting expression representing 'p'
;;     1: degree of 'closedness', how many scopes up the expression references a var.
;;     2: arity of 'p', if 'p' was a lambda, else 0
;;     3: linked list of (p, v) all variables within the expression and their closedness
;;        not sure what use this is...
;;
;; side effects:
;;
;; internal-lambdas          lamref -> body expression value
;; internal-closure-n        lamref -> degree of closedness
;; internal-closure-of       lamref -> optional lamref (parent scope)
;; internal-arity-n          lamref -> arity
;; internal-closures-mine    lamref -> [lamref] (opposite from closure-of)
;; internal-markers          markid -> code
;; internal-marker-known-at  lamref -> [markid]
;;
(define (send-out-lambdas- p parents)
  (cond
    ((lambda-expression? p)
      ;; The meat of what we are doing in this phase
      (let ((lamdid (send-out-lambda p parents)))
        (list (car lamdid) (cadr lamdid) (caddr lamdid) (cadddr lamdid))))

    ((non-resvd-list? p)
      ;; This represents some function application.
      ;; We have code, so we need to push it up to a higher scope if possible.
      ;; This is done by pushing it out into a "marker" on the appropriate scope.
      (let* ((l (p-map (lambda (x) (send-out-lambdas- x parents)) p)))
        (list (p-map car l) (max-list (p-map cadr l)) 0 (apply append (p-map cadddr l)))))

    ((varref? p) (list p (inc (varref-val p)) 0 (list (cons p (inc (varref-val p))))))

    (else (list p 0 0 (list)))))

(define (send-out-lambdas p) (car (send-out-lambdas- p (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Relates a marker m to the lambda l
;; The marker representates a function application.
;; The lambda is the nearest lambda scope containing the expression of m
(define (marker-add-to-lambda! m l)
  (table-set! internal-markers-known-at l (cons m (table-ref internal-markers-known-at l))))

(define (send-out-markers- p parents)
  (cond
    ((lambda-expression? p)
      ;; The meat of what we are doing in this phase
      (let ((lamdid (send-out-lambda p parents)))
        (list (car lamdid) (cadr lamdid) (caddr lamdid) (cadddr lamdid))))

    ((non-resvd-list? p)
      ;; This represents some function application.
      ;; We have code, so we need to push it up to a higher scope if possible.
      ;; This is done by pushing it out into a "marker" on the appropriate scope.
      (let* ((l (p-map (lambda (x) (send-out-lambdas- x parents)) p))
             (m (marker-mk internal-markers-id))
             (thelam (if (null? parents) global-lam (car (car parents))))) ; the scope to bind to
        (table-set! internal-markers internal-markers-id (p-map car l))

        (marker-add-to-lambda! internal-markers-id thelam)

        (set! internal-markers-id (inc internal-markers-id))

        (list m (max-list (p-map cadr l)) 0 (apply append (p-map cadddr l)))))

    ((varref? p) (list p (inc (varref-val p)) 0 (list (cons p (inc (varref-val p))))))

    (else (list p 0 0 (list)))))

(define (send-out-markers p) (car (send-out-markers- p (list))))

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

(define (send-out-preval p) (cons (internal-preval-mk p) true))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
