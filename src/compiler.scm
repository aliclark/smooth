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

;;;;
;;;; BIG CAUTION: fn-sym is NOT equivalent to fn-gen-sym in the compiler.
;;;;              fn-gen-sym should only be inserted when the compiler is generating a lambda.
;;;;              all other lambdas should keep the fn-sym as its head.
;;;;
;;;; step 1: Do minor expansions on defs and fns like (def (f ...) ) and (fn (..) )
;;;; step 2: Start evaluating through the lambdas until we hit primops.
;;;; step 3: Rewrite the remaining lambdas in terms of efficient C code equivalents
;;;;         on a sort of pattern matching and rule basis using intermediate imperative code format.
;;;;         These rules must be easily programmable.
;;;; step 4: straightforward printing of the imperative code format in C syntax.
;;;;
;;;; TODO:
;;;; * Multi file compilation.
;;;; * REPL interpreter with good debugging ability
;;;;   (probably make global definitions mutable and speed not important).
;;;; * Rewrite the Smooth compiler in the Smooth programming language
;;;; * Instead of making the user specify the arity of C functions, parse the C code and find out
;;;; * Find out whether to run init by checking if the name is present in the C code.
;;;; * Add read syntax
;;;; * Add let binding compilation on function calls
;;;;   (The compiler must only evaluate an argument to a function once, the code must not be simply copied)
;;;; * Garbage collection, including iocons, finishers.
;;;;   (I think I'll go with a conservative copying tricolor GC).
;;;; * Multi-threading support? Add some kind of refcounting to GC, stack and pc are thread local
;;;; * TCO - use goto's and continuation k.
;;;;
;;;; NOTES:
;;;; * The module ID symbols must have a strict format of only [a-z][A-Z][0-9], starting with [a-z][A-Z]
;;;;   and hosts starting with "smooth" and "SMOOTH" are not allowed because they clash with our prefix.
;;;;
;;;; !!!!! Infinite recursion due to fnnames not being propogated after alpha-applications !!!!!
;;;;
;;;; Imports specific to Scheme:
;;;;
;;;; equal?        (assoc-ref-nfv, assoc-set, contains?, replace-all)
;;;; eq?           (remove-item, lookup, first-is?)
;;;; string-append (implode-)
;;;; list?         (replace-all, first-is?)
;;;; gensym        (lookup)
;;;; table-ref     (lookup)
;;;; error
;;;;


; We can either try to tag every argument that goes into a function,
; then reduce the code and let-bind any remaining tags,
; or
; we can check through all the calls to our primitives
; and see if the arguments of any two calls are the same.
; This could be astronomically expensive.
; It is O(n.m^2) where n is number of primitives used
; and m is the max number of calls made to any one of them.
; so for 10 different calls and 100 different calls made to one,
; we could have a bound of 100,000.


; For now I'll go with the latter option because it is cheaper for small programs
; and quicker to implement.
;
; now we just need to work out how to save these values that are computed.
;
; We can store values through the cracks of function stack variables
; for the lifetime of that function's call and then hopefully reference back n steps to that memory.

; [a| c |b]
;  ---- --
; Here the function which takes a as it's variable is pushing the intermediate value c before
; pushing the b function local variable. b can now know to reference past it's local variable to it.
; b will also have to make sure to wipe them off the stack before pushing its rv however.
; the value is not popped off immediately because if b makes function calls they
; may want to reference back to it.

;
; First we'll only solve the simple case where the duplicated C calls are not dependant on the stack.
; Then later when we start thinking about callbacks, such as GUI controls,
; we'll need to make sure that dupliated C calls use the above methods
;

; Fairly big issue, the following are not equivalent:
;
; (if test (c_compute a) (c_compute b)) 
; and
; (c_compute (if test a b))
;
; Slightly concerned that this will be used for performance whoaring.
;
; To solve this, we can make all or part of the language lazy,
; but that may add different performance inequalities in recursive codes.
;
; I could make a concept called "trips", which would be like:
;   struct smooth_trip { smooth_t code; smooth_t value; }
; where value itself could be another trip.
; The idea is that these get passed around and when the time comes they can be evaluated.


; ((c_test z) (c_compute x)) (c_compute y))  // c_test is a closure object which we want to apply lazily.
; We have no idea whether (c_test z) is lazy until it is created.
;
; The tradeoff fairly much is between (if test (delay (c_compute a)) (delay (c_compute a)))
; and (if (null? xs) 0 (force (+ 1 (length xs))))
;
; Or can we do `smooth_t my_if (strict_t x, strict_t y);` in module code instead?
;    or better `delay_t my_test (strict_t x);` ?
; still doesn't seem like enough

; Types of functions we can find on stack - any except for primops:
;
; lambda, smooth_t(*f)(smooth_t), closure_t*
;
; Expecting that primops won't be passed onto stack as straight f*'s
; (although this is valid syntax if the primop happens to have arity of 1).
;
; We now need to also make lambda calls strict for the case of using loops at run-time.
; As we already know, closure_t* must be lazy to allow c_test to return a bool closure.
; It is not so important whether f* is strict yet, but we can assume it is.
; Now, how can we assure that we have correct strictness for different function types on stack?
;
; arity of c_test = 1
; arity of c_putc = 3
;
; we know that ((c_test z) (c_compute x) (c_compute y)) calls from stack but (c_putc f c r) does not.
;
; therefore we use this code:
;
; push y
; delay c_compute
; push x
; delay c_compute
; push z
; call c_test
; stack_call()  // we know this is special and can use different call mechanism
; stack_call()
;
; Now we just have to make sure all lambda calls remain strict and only closures are affected.
;
; (map f xs) = (if (null? xs) nil (cons (f (car xs)) (map f (cdr xs))))
; (map somelambda (c_getlist x))
;
; Question: will (f (car xs)) func up?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MISC LIBRARY CODEZ

(: (inc x) (+ x 1))

(: (set-diff a b)
  (if (null? b) a
    (if (contains? a (car b))
      (remove-item a (car b))
      (set-diff a (cdr b)))))

(: (assoc-ref-nfv d n not-found-value)
  (if (null? d)
    not-found-value
    ((lambda (nex)
      (if (equal? (fst nex) n)
        (snd nex)
        (assoc-ref-nfv (tail d) n not-found-value)))
      (head d))))

(: (assoc-set lst key val)
  (if (null? lst)
    (cons (pair key val) nil)
    ((lambda (next)
      (if (equal? key (fst next))
        (cons (pair key val) (tail lst))
        (cons next (assoc-set (tail lst) key val))))
      (head lst))))

(: (contains? l x)
  (if (null? l) false (if (equal? (head l) x) true (contains? (tail l) x))))

(: (ensure-contains l x) (if (contains? l x) l (cons x l)))

(: (ensure-merge l xs)
  (if (null? xs) l
    (ensure-merge (ensure-contains l (head xs)) (tail xs))))

(: (remove-item xs y)
  (if (null? xs) nil
    (if (eq? (head xs) y) (remove-item (tail xs) y) (cons (head xs) (remove-item (tail xs) y)))))

(: (uniques l)
  (if (null? l) nil
    (if (contains? (tail l) (head l))
      (uniques (tail l))
      (cons (head l) (uniques (tail l))))))

(: (replace-all l x v)
  (if (list? l)
    (map (lambda (l2) (replace-all l2 x v)) l)
    (if (eq? l x) v l)))

(: (lookup hsh expression)
  ((lambda (fail)
    ((lambda (l)
      (if (eq? l fail)
        (pair false expression)
        (pair true l)))
      (table-ref hsh expression fail)))
    (gensym)))

(: (first-is? sym l) (and (list? l) (and (not (null? l)) (eq? (head l) sym))))

(: (implode- acc glue l)
  (if (null? l) acc
    (implode-
      (string-append acc
        (if (null? (tail l)) (head l) (string-append (head l) glue)))
      glue
      (tail l))))

(: (implode glue l) (implode- "" glue l))

(: (string-find x y)
  (let ((xlen (string-length x)) (ylen (string-length y)))
    (let loop ((curx 0) (cury 0))
      (if (= cury ylen)
        (- curx ylen)
        (if (= curx xlen)
          false
          (if (char=? (string-ref x curx) (string-ref y cury))
            (loop (inc curx) (inc cury))
            (loop (inc curx) 0)))))))

(: (string-replace s a b)
  (let ((alen (string-length a)))
    (let ((f (string-find s a)))
      (if f
        (string-append (substring s 0 f) b
          (string-replace (substring s (+ f alen) (string-length s)) a b))
        s))))

(: (read-line d)
  (if (eof-object? (peek-char d))
    (peek-char d)
    (let loop ((acc nil))
      (let ((c (read-char d)))
        (if (or (eof-object? c) (eq? c #\newline))
          (list->string (reverse acc))
          (loop (cons c acc)))))))

(: (read-lines d)
  (let loop ((acc nil))
    (let ((l (read-line d)))
      (if (eof-object? l)
        (reverse acc)
        (loop (cons l acc))))))

(: (read-all file)
  (let ((ip (open-input-file file)))
    (let loop ((acc nil))
      (let ((r (read ip)))
	(if (eof-object? r)
	  (begin (close-input-port ip) (reverse acc))
	  (loop (cons r acc)))))))

(: (read-all-files files) (apply append (map read-all files)))

(: (n-of x n) (if (= n 0) nil (cons x (n-of x (- n 1)))))

(: (every pred v) (if (null? v) #t (and (pred (car v)) (every pred (cdr v)))))
(: (any   pred v) (if (null? v) #f (or  (pred (car v)) (any pred (cdr v)))))

(: (listndeep cpy it n) (if (= n 0) it (list cpy (listndeep cpy it (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOMAIN SPECIFIC CODEZ

(: begin-sym 'begin)
(: def-sym   ':)
(: fn-sym    (string->symbol "\\"))
(: main-sym  'main)

;; Since the compiler is going to be inserting lambdas,
;; we need to use a symbol for them that can't be shadowed.
(: fn-gen-sym (gensym fn-sym))

(: macros  (make-table))
(: primops (make-table))

(: (macroexpand shadow-list form)
  (if (and (list? form) (not (null? form))
        (not (keyword-is-shadowed? (head form) shadow-list)))
    (let ((v (table-ref macros (head form) false)))
      (if v
        (macroexpand shadow-list (apply v (tail form)))
        form))
    form))
 
(: (is-begin?  thing) (first-is? begin-sym thing))
(: (is-def?    thing) (first-is? def-sym   thing))
(: (is-fn?     thing) (or (first-is? fn-sym thing) (is-gen-fn? thing)))
(: (is-gen-fn? thing) (first-is? fn-gen-sym thing))

(: (is-primop? thing) (table-ref primops thing false))

(: (keyword-is-shadowed? word slist) (contains? slist word))

(: (begin-is-shadowed? slist) (keyword-is-shadowed? begin-sym slist))
(: (def-is-shadowed?   slist) (keyword-is-shadowed? def-sym   slist))
(: (fn-is-shadowed?    slist) (keyword-is-shadowed? fn-sym    slist))

(: (primop-arity name) (cdr (table-ref primops name)))
(: (primop-base  name) (car (table-ref primops name)))
(: (primop-path  name) (string-append (car (table-ref primops name)) "__" (symbol->string name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing
;;; * ( and ) start / end a list
;;; * Whitespace (spaces and newlines) seperates items
;;; * The hash character # starts a read macro after which different rules may apply
;;; that's it.

; char=?, cspace, cnewline, ctab, coparen, ccparen, make-table
; peek-char, read-char, string->symbol, list->string, eof-object?, error

(: (white-space? c) (and (not (eof-object? c)) (or (char=? c cspace) (or (char=? c cnewline) (char=? c ctab)))))
(: (open-paren?  c) (and (not (eof-object? c)) (char=? c coparen)))
(: (close-paren? c) (and (not (eof-object? c)) (char=? c ccparen)))
(: (paren?       c) (or (open-paren? c) (close-paren? c)))

(: read-table (make-table))

(: (strip-spaces! p)
  ((lambda (c)
    (if (white-space? c)
      (begin (read-char p) (strip-spaces! p))
      false))
    (peek-char p)))

(: (reads p s)
  ((lambda (c)
    (if (or (white-space? c) (paren? c))
      (string->symbol (list->string (reverse s)))
      (begin (read-char p) (reads p (cons c s)))))
    (peek-char p)))

(: (read-symbol p) (reads p (cons (read-char p) nil)))

(: (readl p l)
  (begin
    (strip-spaces! p)
    ((lambda (c)
      (if (eof-object? c) (error "Incomplete list")
        (if (open-paren? c) (readl p (cons (read-list p) l))
          (if (close-paren? c) (begin (read-char p) (reverse l))
            (readl p (cons (read-symbol p) l))))))
      (peek-char p))))

(: (read-list p) (begin (read-char p) (readl p nil)))

(: (read p)
  (begin
    (strip-spaces! p)
    ((lambda (c)
      (if (eof-object?  c) c
        (if (open-paren?  c) (read-list p)
          (if (close-paren? c) (error "Not in list")
            (read-symbol p)))))
      (peek-char p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expansion phase - rename variables to unique symbols and fully expand the forms.

;; (fn () exp)           -> exp
;; (fn (v vs...) exp...) -> (fn v (fn (vs...) (exp...)))
(: (expand-fn shadow-list thing)
  (cond
    ((< (length thing) 3)
      (error "Too few items in lambda expression: " thing))
    ((> (length thing) 3)
      (expand-fn shadow-list (cons (head thing) (cons (cadr thing) (cons (cons (cddr thing) nil) nil)))))
    ((null? (cadr thing)) (caddr thing))
    ((not (list? (cadr thing)))
      (let ((v (gensym (cadr thing))))
        (cons (head thing)
          (cons v
            (cons
              (replace-all
                (expand-expression (ensure-contains shadow-list (cadr thing)) (caddr thing))
                (cadr thing)
                v)
              nil)))))
    ((null? (cdadr thing))
      (expand-fn shadow-list (list (head thing) (caadr thing) (caddr thing))))
    (else
      (expand-fn shadow-list (list (head thing) (caadr thing) (list fn-gen-sym (cdadr thing) (caddr thing)))))))

(: (expand-single-apps xs)
  (if (null? (cddr xs))
    xs
    (cons (expand-single-apps (butlast xs)) (cons (last xs) nil))))

;; Here we create a lambda of the same arity as the primop to ensure that
;; the primop is always called only when it has the right no. of args.
(: (expand-primop-arity tmp n)
  (if (zero? n) tmp
    (let ((newvar (gensym)))
      (list fn-gen-sym newvar (expand-primop-arity (list tmp newvar) (- n 1))))))

(: (expand-expression shadow-list thing)
  (let ((ex (macroexpand shadow-list thing)))
    (if (null? ex)
      (error "Empty list is invalid.")
      (if (and (list? ex) (not (null? ex)) (null? (tail ex)))
        (expand-expression shadow-list (head ex))
        (if (or (is-gen-fn? ex) (and (not (fn-is-shadowed? shadow-list)) (is-fn? ex)))
          (expand-fn shadow-list ex)
          (if (list? ex)
            (expand-single-apps (map (partial expand-expression shadow-list) ex))
            (if (or (keyword-is-shadowed? ex shadow-list))
              ex
              (if (is-primop? ex)
                (expand-primop-arity ex (primop-arity ex))
                (error "Unbound variable: " ex)))))))))

;; (def (f xs...) exp...) -> (def f (fn (xs...) (exp...)))
(: (expand-def thing)
  (cond
    ((< (length thing) 3)
      (error "define form does not have enough body: " thing))
    ((> (length thing) 3)
      (expand-def (cons (head thing) (cons (cadr thing) (cons (cons (cddr thing) nil) nil)))))
    ((not (list? (cadr thing))) thing)
    ((null? (cadr thing)) (error "Empty list in code: " thing))
    ((null? (cdadr thing))
      (expand-def (cons (head thing) (cons (caadr thing) (cons (caddr thing) nil)))))
    (else
      (expand-def
        (cons (head thing)
          (cons (caadr thing) (cons (cons fn-gen-sym (cons (cdadr thing) (cons (caddr thing) nil))) nil)))))))

(: (expand-expressions-in-defs shadow-list codes)
  (let ((newvars (map cadr codes)))
    (if (= (length (uniques newvars)) (length newvars))
      (let ((newscope (ensure-merge shadow-list newvars)))
	(map
	  (lambda (code)
	    (cons (head code) (cons (cadr code) (cons (expand-expression newscope (cddr code)) nil))))
	  codes))
      (error "Multiple definitions for a variable of the same name" (set-diff newvars (uniques newvars))))))

(: (expand-statement shadow-list thing)
  (let ((ex (macroexpand shadow-list thing)))
    (cond
      ((and (not (begin-is-shadowed? shadow-list)) (is-begin? ex))
        (expand-statements shadow-list (tail thing)))
      ((and (not (def-is-shadowed? shadow-list)) (is-def? ex))
        (cons (expand-def ex) nil))
      (else (error "Not a define form at top level" ex)))))

(: (expand-statements shadow-list code)
  (expand-expressions-in-defs shadow-list
    (foldr append nil (map (partial expand-statement shadow-list) code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduction - move the code into hash table and reduce main expression

(: (codes->hash codes)
  (let ((hsh (make-table)))
    ;; load the code into the hash table.
    (foldr (lambda (x acc) (table-set! hsh (cadr x) (caddr x)) 'done) 'done codes)
    hsh))

(: (alpha-apply f v) (replace-all (caddr f) (cadr f) v))

(: (reducex-find-culprit f) (if (list? f) (reducex-find-culprit (head f)) f))

(: (reducex-add-lookup-f expression f)
  (if (list? (head expression))
    (cons (reducex-add-lookup-f (head expression) f) (tail expression))
    (cons f (tail expression))))

(: (reducex-rec-app hsh exnames fnnames locals tamep expression rex nexrex)
  (let* ((f    (snd rex))
         (cul  (reducex-find-culprit f))
         (newf (reducex hsh (remove-item exnames cul) fnnames locals true cul))
         (rv   (reducex hsh exnames (reducex-do-app fnnames f (snd nexrex)) locals false
                 (reducex-add-lookup-f expression (snd newf)))))
    (pair (ensure-merge (fst rv) (ensure-merge (fst rex) (fst nexrex))) (snd rv))))

(: (reducex-been-applied? fnnames f arg)
  (contains? (assoc-ref-nfv fnnames f nil) arg))

(: (reducex-do-app fnnames f arg)
  (assoc-set fnnames f (ensure-contains (assoc-ref-nfv fnnames f nil) arg)))

;;; TODO : finish implementation !!!!!!!!!!!!!!!
(: (rfnapps-merge a b)
  b)
;  (if (null? a) b (rfnapps-merge (tail a) (dosummat (head a) b))))

(: (reducex-gensym-locals f)
  (if (is-fn? f)
    (let ((v (gensym (cadr f))))
      (cons (head f) (cons v (cons (replace-all (reducex-gensym-locals (caddr f)) (cadr f) v) nil))))
    (if (list? f)
      (map reducex-gensym-locals f)
      f)))

(: (robj deps exp fnapps) (cons deps (cons exp (cons fnapps nil))))
(: rdeps   head)
(: rexp    cadr)
(: rfnapps caddr)

;; if this is a recursive application that has already been done, we need to remember the
;; symbol as one of the dependancies.
;; but if (map ++) has been done but ((map ++) (cons 1 nil)) hasn't, we don't want to remember `map`.

(: (reducex-application hsh exnames fnnames locals tamep expression)

  (if (and (symbol? (head expression)) (reducex-been-applied? fnnames (head expression) (cadr expression)))

    ;; still need to propogate dependancy info
    (let ((nexrex (reducex hsh exnames fnnames locals tamep (cadr expression))))
      (robj (rdeps nexrex) (cons (head expression) (cons (rexp nexrex) nil)) (rfnapps nexrex)))

    (let* ((rex (reducex hsh exnames fnnames locals true (head expression)))
           (f   (rexp rex)))
    
      (if (is-fn? f)
        (if tamep
          (let ((exp (alpha-apply f (cadr expression))))
            (if (is-fn? exp)
              (robj (rdeps rex) exp (rfnapps rex))
              (let ((nexrex (reducex hsh exnames fnnames locals tamep exp)))
                (robj (ensure-merge (rdeps rex) (rdeps nexrex)) (rexp nexrex)
                  (rfnapps-merge (rfnapps rex) (rfnapps nexrex))))))
          
          (let ((nexrex
                 (reducex hsh exnames
                   (if (symbol? (head expression)) (reducex-do-app fnnames (head expression) (cadr expression)) fnnames)
                   locals
                   tamep
                   (alpha-apply f (cadr expression)))))
            (robj (ensure-merge (rdeps rex) (rdeps nexrex)) (rexp nexrex)
              (rfnapps-merge (rfnapps rex) (rfnapps nexrex)))))
        
        (let ((nexrex (reducex hsh exnames fnnames locals false (cadr expression))))
          (robj (ensure-merge (rdeps rex) (rdeps nexrex)) (cons f (cons (rexp nexrex) nil))
            (rfnapps-merge (rfnapps rex) (rfnapps nexrex))))))))

;(: baz (\ x (\ y ((baz y) y))))
;((baz y) y)

(: (reducex hsh exnames fnnames locals tamep expression)
  (cond

    ((is-fn? expression)
      (if tamep
        (robj nil expression nil)
        (let ((rex (reducex hsh exnames fnnames (ensure-contains locals (cadr expression)) tamep (caddr expression))))
          (robj (rdeps rex) (cons (head expression) (cons (cadr expression) (cons (rexp rex) nil))) (rfnapps rex)))))

    ((list? expression)             (reducex-application hsh exnames fnnames locals tamep expression))
    ((contains? locals expression)  (robj nil expression nil))
    ((contains? exnames expression) (robj (cons expression nil) expression nil))

    (else
      (let ((v (lookup hsh expression)))
        (cond
          ((fst v)
            (reducex hsh (cons expression exnames) fnnames locals tamep
              (if (is-fn? (snd v)) (reducex-gensym-locals (snd v)) (snd v))))
          ((is-primop? expression) (robj nil expression nil))
          (else                    (error "Eek! Unbound variable: " expression)))))))

(: (reduce-dependancies redhsh hsh deps)
  (if (null? deps)
    redhsh
    (let ((fail (gensym)))
      (if (eq? (table-ref redhsh (head deps) fail) fail)
	(let ((rex (reducex hsh nil nil nil false (head deps))))
	  (table-set! redhsh (head deps) (rexp rex))
	  (reduce-dependancies (reduce-dependancies redhsh hsh (rdeps rex)) hsh (tail deps)))
	(reduce-dependancies redhsh hsh (tail deps))))))

(: (main-reduce codes)
  (reduce-dependancies (make-table) (codes->hash codes) (cons main-sym nil)))

(: (reduce-all codes)
  (reduce-dependancies (make-table) (codes->hash codes) (map cadr codes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Optimize phase -
;;; Translate into imperative instruction form and slow sequences with faster ones.
;;; Takes a code hash as input and returns list of imperative code.

(: (find-first p xs)
  (if (null? xs) #f (if (p (car xs)) (car xs) (find-first p (cdr xs)))))


(: todofns '())
(: todons  1)

(: (todofns-mk id code closdp) (list id code closdp))
(: (todofns-id x)     (car x))
(: (todofns-code x)   (cadr x))
(: (todofns-closdp x) (caddr x))

;; (\ x (\ y y))
;; (\ a (\ b b))
;; x = a
;; y = b
(: (todofns-same-code-? x y vars)
  (cond
    ((is-fn? x)
      (if (is-fn? y)
        (todofns-same-code-? (caddr x) (caddr y) (assoc-set vars (cadr x) (cadr y)))
        #f))
    ((and (list? x) (list? y))
      (and
        (todofns-same-code-? (car  x) (car  y) vars)
        (todofns-same-code-? (cadr x) (cadr y) vars)))
    ((and (symbol? x) (symbol? y)) (eq? (assoc-ref-nfv vars x '()) y))
    (else #f)))

(: (todofns-same-code? x y) (todofns-same-code-? x y '()))

(: (todofns-same-closdp? x y) (or (and x y) (not (or x y))))

(: (todofns-lookup code closdp)
  (find-first
    (lambda (f)
      (and
        (todofns-same-code? (todofns-code f) code)
        (todofns-same-closdp? (todofns-closdp f) closdp)))
    todofns))

(: (todofns-register code closdp)
  (let ((fn (todofns-lookup code closdp)))
    (if fn
      (todofns-id fn)
      (begin
        (set! todofns (cons (todofns-mk todons code closdp) todofns))
        (set! todons (+ todons 1))
        (- todons 1)))))

(: todo-evvs '())
(: todo-evns 0)

(: (todo-evvs-mk id code closdp) (list id code closdp))
(: (todo-evvs-id x)     (car x))
(: (todo-evvs-code x)   (cadr x))
(: (todo-evvs-closdp x) (caddr x))

(: cquote-sym (gensym))
(: (is-cquote? s) (and (pair? s) (eq? (car s) cquote-sym)))
(: (cquote x)   (cons cquote-sym x))
(: (cquote-value x) (cdr x))

;; The only reason to not be eval ready is if we reference a local variable on stack.
;; if the leaf is a symbol and not a primop, then false.
(: (is-evalready? v)
  (cond
    ((or (is-fn? v) (is-cquote? v)) #f)
    ((list? v)  (every (lambda (x) (or (is-fn? x) (is-evalready? x))) v))
    (else       (or (not (symbol? v)) (is-primop? v)))))

(: (replace-locals code locals)
  (let loop ((c code) (l locals) (y 0))
    (if (null? l) c
      (loop (replace-all c (car l) (list 'LOCAL y)) (cdr l) (+ y 1)))))

(: (send-out-fns v closdp)
  (if (is-fn? v)
    (todofns-register v closdp)
    (if (list? v)
      (map (lambda (x) (send-out-fns x closdp)) v)
      v)))


; Some examples of using prevals within prevals.
;
; (: main (run (>>= (getcc stdin) (putcc stdout))))
;
; smooth_preval_3 = cgetc(stdin, ~4~);
; smooth_preval_2 = iocons_cdr(smooth_preval_3);
; smooth_preval_1 = cputc(stdout, iocons_car(smooth_preval_3), ~2~);
;
; (: main (run (>>= getchar putchar)))
;
; smooth_preval_4 = cgetc(stdin, ~5~);
; smooth_preval_3 = iocons(ulint_to_numeral(iocons_car(smooth_preval_4)), iocons_cdr(smooth_preval_4));
;
; smooth_preval_2 = iocons_cdr(smooth_preval_3);
; smooth_preval_1 = cputc(stdout, numeral_to_ulint(iocons_car(smooth_preval_3)), ~3~);
;

; Slightly less important, but we also want to quickly go through the fns to check if any are duplicates
; of the same code.

; Also we should re-order the printing of fns to get a better locality

; We need to be able to go back over some code we have already seen and patch in a preval.
; This means we need to be weary of creating more lambdas than we need.

; We need to go through the fns sending them out.


;; This will allow us to replace eg. (numeral_to_ulint (\ ...)) with (cquote 44)
;; before the function gets sent onto the todo list.
(: (primop-call-optimisable? v) #f)
(: (primop-call-optimise v)     v)

(: (docode v closdp)
  (cond
    ((is-cquote? v) (list (list 'SMOOTH_CALL (cquote-value v))))
    ((is-fn? v)
      (list (list 'SMOOTH_PUSH (todofns-register v closdp))))

    ((and (list? v) (is-evalready? v))
      (set! todo-evvs (cons (todo-evvs-mk todo-evns (send-out-fns v closdp) closdp) todo-evvs))
      (set! todo-evns (+ todo-evns 1))
      (list (list 'SMOOTH_PUSH (string-append "smooth_preval[" (number->string (- todo-evns 1)) "]"))))

    ;; If we reach here we have a function call, so we pop the arguments first.
    ((list? v)
      (if (= (length v) 1)
        (docode (car v) closdp)
        (if (and (is-primop? (head v)) (primop-call-optimisable? v))
          (primop-call-optimise v)
          (append
            (cond
              ((is-cquote? (cadr v)) (list (list 'SMOOTH_PUSH (cquote-value (cadr v)))))
              ((and (not (is-fn? (cadr v))) (list? (cadr v)))
                (docode (cadr v) closdp))
              ((is-primop? (cadr v))
                (list (list 'SMOOTH_PUSH (primop-path (cadr v)))))
              ((is-fn? (cadr v))
                (list (list 'SMOOTH_PUSH (todofns-register (cadr v) closdp))))
              (else (list (list 'SMOOTH_PUSH (cadr v)))))
            (docode (head v) closdp)))))
    ((is-primop? v)
      (list (list (string-append "PRIMCALL_" (number->string (primop-arity v))) (primop-path v))))
    (else (list (list 'SMOOTH_PUSH v)))))

;; When we encounter a new variable that must be closed over,
;; we ensure first that we convert references to those expressions into
;; a call up the closure chain to get the right variable.
(: (docode-lambda v closdp)
  (let ((s (if closdp '((SMOOTH_SET SELF (SMOOTH_CLOSURE_CAST (SMOOTH_POP)))) '((SMOOTH_SPDEC)))))
    (append s '((SMOOTH_SET LOCAL (SMOOTH_POP)))
      (if (is-fn? v)
        `((SMOOTH_PUSH
          (SMOOTH_CLOSURE_CREATE ,(todofns-register v true) LOCAL ,(if closdp 'SELF 'NULL))))
	(docode v true)))))

(: (exdepth x) (cond ((is-fn? x) 0) ((list? x) (+ (max (exdepth (cadr x)) (- (exdepth (car x)) 1)) 1)) (else 0)))

(: (clos-lookup n)
  (cquote
    (if (= n 0)
      'LOCAL
      `(SMOOTH__CLOSURE_LOCAL ,(listndeep 'SMOOTH__CLOSURE_PARENT 'SELF (- n 1))))))

(: (depth-align-varn expression vname n)
  (cond
    ((is-fn? expression)
      (list (car expression) (cadr expression)
	    (depth-align-varn (caddr expression) vname (+ n 1))))
    ((list? expression) (map (lambda (x) (depth-align-varn x vname n)) expression))
    ((eq? expression vname) (clos-lookup n))
    (else expression)))

(: (depth-align-var expression vname) (depth-align-varn expression vname 0))

(: (lambda-depths tc)
  (let ((tab (make-table)) (l (length tc)))
    (map (lambda (x) (table-set! tab (car x) (number->string (cadr x)))) tc)
    (let loop ((i 0) (accs '()))
      (if (= i l) (reverse accs)
        (loop (+ i 1) (cons (table-ref tab i) accs))))))

;;;((a b) c)
;(iocons_cdr ((cputchar (numeral_to_ulint '+')) 1))

; (((cputc stdout)
;  (numeral_to_ulint 8))
;  9)

(: (re-aritise-head-depth e) (if (list? e) (+ (re-aritise-head-depth (car e)) 1) 0))
(: (re-aritise-head-arity e) (if (list? e) (re-aritise-head-arity (car e)) (primop-arity e)))

(: (re-aritise-flatten x)
  (if (list? (car x))
    (append (re-aritise-flatten (car x)) (list (re-aritise (cadr x))))
    (list (car x) (re-aritise (cadr x)))))

(: (re-aritise e)
  (if (list? e)
    (append
      (if (list? (car e))
        (if (< (re-aritise-head-depth (car e)) (re-aritise-head-arity (car e)))
          (re-aritise-flatten (car e))
          (list (car e)))
        (list (car e)))
      (list (re-aritise (cadr e))))
    e))

(: (prim-full-paths c)
  (if (list? c) (map prim-full-paths c)
    (if (symbol? c) (primop-path c) c)))

(: (do-evv e)
  `(SMOOTH_SET ,(string-append "smooth_preval[" (number->string (todo-evvs-id e)) "]")
     ,(tocstr (todo-evvs-code (prim-full-paths (re-aritise e))))))

(: (docode-reroll c)
  (if (or (null? c) (null? (cdr c))) c
    (let loop ((n 1) (nex (car c)) (rem (cdr c)))
        (if (null? rem)
          (if (= n 1) (list nex) (list (list 'SMOOTH_REPEAT nex n)))
          (if (equal? (car rem) nex)
            (loop (+ n 1) nex (cdr rem))
            (cons
              (if (= n 1) nex (list 'SMOOTH_REPEAT nex n))
              (loop 1 (car rem) (cdr rem))))))))


(: (docode-complete v)
  (let ((dc (docode-reroll (docode v false))))
    (let loop ((ac (list (cons 0 (cons (exdepth v) dc)))))
      (if (null? todofns)

        ;; Now the fns are done we go through the preval code and add it to the front.
        (cons (list todo-evns (map do-evv todo-evvs)) (reverse ac))

        ;; Go through the lambdas generating code.
        ;; This may create even more lambdas to work through in the process
        ;; and likewise will add code to the todo-evvs list
        (let ((nex (car todofns)))
          (set! todofns (cdr todofns))
          (loop
            (cons
              (cons (todofns-id nex)
                (cons (exdepth (caddr (todofns-code nex)))
                  (docode-reroll
                    (docode-lambda
                      (depth-align-var (caddr (todofns-code nex)) (cadr (todofns-code nex)))
                      (todofns-closdp nex)))))
              ac)))))))

(: (optimize-equivalents tb)
  (docode-complete (table-ref tb main-sym)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate phase - simply take the imperative instruction list and print as C.

(: (tocstr x)
  (cond
    ((is-cquote? x) (tocstr (cquote-value x)))
    ((string? x) x)
    ((symbol? x) (symbol->string x))
    ((number? x) (number->string x))
    ((list? x)   (string-append (tocstr (head x)) "(" (implode ", " (map tocstr (tail x))) ")"))))

(: (instructions-string c i)
  (implode "" (map (lambda (x) (string-append (if i "      " "  ") (tocstr x) ";\n")) c)))

;; This should take the imperative code format and simply print it out as C.
(: (generate-c tbc)
  (let ((tc (cdr tbc)) (td (caar tbc)) (tv (cadar tbc)))

    (string-append "
#include \"smoothlang/anc2020/_smooth.h\"

"
(apply string-append
  (map
    (lambda (p)
      (string-append "extern smooth_t " (cadr p) "__" (symbol->string (car p))
        (if (= (cddr p) 0) "" (string-append " (" (implode ", " (n-of "smooth_t" (cddr p))) ")"))
        ";\n"))
    (table->list primops)))
"
static smooth_t smooth_preval[" (number->string td) "];

void smooth_execute (void) {
  unsigned long int i;
  smooth_closure_t* self;
  smooth_t local;
"
(if (> (length (cdr tc)) 1)
(string-append "
#if 0
jump:
#endif
  switch (smooth_pc) {
"
(implode ""
  (map
    (lambda (c)
      (string-append "    case " (number->string (head c)) ":\n"
        (instructions-string (cddr c) true)
        "      break;\n"))
    (cdr tc)))
"  }")
(instructions-string (cdaadr tc) false))
"
}

int main (const int argc, const char** const argv) {

  smooth_argc = (smooth_t) argc;
  smooth_argv = (smooth_t) argv;

#ifndef SMOOTH_FIXED_STACK
  smooth_stack = smooth__linked_array_allocate(SMOOTH_STACK_SIZE);
#endif

"
(instructions-string (append tv (cddr (car tc))) false)
"
  return EXIT_SUCCESS;
}

")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module structure

;; All deps file must use forward slash on both Unix and Windows.
(: the-slash "/")

;; Currently only Unix :)
(: system-slash "/")

(: (end-with-slash s)
  (let ((sl (string-length s)))
    (if (= sl 0)
      system-slash
      (if (eq? (string-ref s (- sl 1)) system-slash)
        s
        (string-append s system-slash)))))

;; Currently only supports one path.
(: (get-lib-paths) (map end-with-slash (list (getenv "SMOOTH_PATH"))))

(: (code-path-exists filepart libpath)
  (open-input-file (string-append libpath filepart filext)))

;; smooth-lang/anc2020/mybar
;; "/smooth-lang/anc2020/my bar" (foo)
;;
;; Currently only supports the first format
(: (compile-dep s)
  (let ((filepart (string-replace s the-slash system-slash)))
    (find-first (partial code-path-exists filepart) (get-lib-paths))))

;; Queries the server and downloads any updated files.
(: (smosync-all) false)
(: (smosync-file file recursivep forcep) false)

;; file has format "smooth-lang/anc2020/list"
;; If it is recursive, it will also download the dependancies for list
;; but it will only overwrite existing dependancy files if we use forcep.
(: (smoinstall file recursivep forcep) false)

(: (smooth-path-to-codefile x) (string-append "~/code/smooth/" x ".smo"))
(: (smooth-path-to-libfile  x) (string-append "~/code/smooth/" x ".slo"))

(: (code-header c)
  (if (null? c) nil
    (if (or (is-begin? (head c)) (is-def? (head c))) nil
      (cons (head c) (code-header (tail c))))))

(: (code-forms c)
  (if (null? c) nil
    (if (or (is-begin? (head c)) (is-def? (head c))) c
      (code-forms (tail c)))))

(: (has-main-sym? c) (not (eq? (table-ref (codes->hash c) main-sym false) false)))

(: (the-imports  h) nil)

(: (the-includes h)
  (if (null? h) nil
    (if (eq? (caar h) 'include)
      (cons (car h) (the-includes (cdr h)))
      (the-includes (cdr h)))))

(: (the-primops  h) (map cdr (the-includes h)))

(: (add-codez x rv)
  (read-all (smooth-path-to-libfile x)))

(: (compile-executable h c)
  (map compile-entry (the-imports h))
  ;;now include the libraries codes.
  (let ((c (foldr add-codez c (the-imports h))))
    (let* ((p2t (main-reduce (expand-statements nil c))))
      (generate-c (optimize-equivalents p2t)))))

(: (compile-library h c)
  (map compile-entry (the-imports h))
  (table->list (reduce-all (expand-statements nil c))))

(: (primop-path-to-c p) (string-replace p "/" "_"))

(: (add-primops! h)
  (map
    (lambda (x)
      (map
        (lambda (y)
          (let ((name (if (list? y) (car y) y))
                (arity (if (list? y) (cadr y) (string->symbol "0"))))
            (table-set! primops name
              (cons (primop-path-to-c (symbol->string (car x)))
                (string->number (symbol->string arity))))))
        (cdr x)))
    (the-primops h)))

(: (compile-entry x)
  (let* ((a (read-all (smooth-path-to-codefile x)))
         (h (code-header a)))
    ;; Add the primops.
    (add-primops! h)
    (let ((c (expand-statements nil (code-forms  a))))

      (if (has-main-sym? c)
        (display (compile-executable h c))
        (let ((l (compile-library h c))
              (lf (open-output-file (smooth-path-to-libfile x))))
          (display l lf)
          (close-output-port lf))))))

(if (null? (tail (command-line))) false (compile-entry (cadr (command-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
