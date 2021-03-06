
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-arity-values)
  (implode ", " (map (lambda (x) (number->string (+ (cdr (car x)) (max (cdr (cdr x)) 0))))
                  (zip (reverse (table->list internal-arity-n)) (reverse (table->list internal-closure-n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;
; we could consider each and every value as having its own type 
; the only way i could see this playing in usefully is that we might
; be able to then overlap the calling-into and the calling-out area.
; so if
; f x y = g y x
;
; normally we have
; push y
; push x
; call f
; ->
; push arg[0]
; push arg[1]
; call g
;
; but maybe we could do
;
; push x
; push y
; call f
; goto g

; note that this is all about reducing the number of times the same data has to be swapped around
; to match differing argument patterns
; i guess this is a sort of register allocation problem on mapping stack variables to specific argument slots.
; the idea is that we could use the hotspot idea from JIT's to see which sites tend to get which type of function
; then on the fly we compile fast code for exactly those kind of calls.
;
; perhaps instead of moving things to the right position beforehand and then calling with a mapping 0->arg[0],1->arg[1],...
; we could leave things where they are and call with the mapping 0->1,1->0, and allow the function to look up arg[1]
; seems pretty crummy.



(define (itemstr- x id suppress)
  (cond
    ((symbol? x) (symbol->string x))
    ((string? x) x)
    ((number? x) (number->string x))
    ((tmpref? x) (string-append "tmp[" (number->string (tmpref-id x)) "]"))
    ((varref? x) (string-append "(x === null) ? args[bp + " (number->string (varref-val x)) "] : xbl[" (number->string (varref-val x)) "]"))
    ((localref? x) (string-append "locals[" (number->string (localref-id x)) "]"))
    ((lamref? x)
      (if (lamref-is-closure? x)
        (string-append "closure_single(" (number->string (lamref-id x)) ", VARIABLE_LOOKUP(0))")
        (number->string (lamref-id x))))
    ((preval? x) (string-append "preval[" (number->string (preval-id x)) "]"))
    ((primop-symbol? x) (primop-to-id x))
    ((non-resvd-list? x)
      (cond
        ((eq? (car x) 'SET)
          (string-append id (itemstr- (cadr x) "" true) " = " (itemstr- (caddr x) "" true) ";\n"))
        ((eq? (car x) 'APPLY) (itemstr- (cons 'smoothApply (cdr x)) id suppress))
        ((eq? (car x) 'TAIL_APPLY) (string-append "
                            // Tail apply follows. Surely all we need is code to distinguish pushing
                            // pushing a new frame as opposed to not... seems like overkill here...
                            xc = " (itemstr- (cadr x) "" true) "

                            xct = xc.constructor;

                            if (xct === Object)
                            {
                                x   = xc;
                                xc  = x.c;
                                xp  = x.p;
                                xb  = x.b;
                                xbl = xb.l;
                                copy = (xp === " (number->string (cadddr x)) ");
                            }
                            else
                            {
                                x = null;
                                copy = ((xct === Number) && (smoothLambdaSizes[xc] === " (number->string (cadddr x)) "));
                            }

                            if ((x !== null) && copy)
                            {
                                // xbl[1] = arg1;
                                // xbl[0] = arg2;
                                // ...
                                xb.p   = 0;

                                // NOTE: it would normally need a new head (valid xp count),
                                // but we lose reference to it anyway, very subtle...
                            }
                            else
                            {
                                // put the args onto stack
                                //
                                // args[bp + 1] = arg1;
                                // args[bp + 0] = arg2;
                                // ...
                            }
                            if (copy)
                            {
                                continue loop;
                            }
                            bp += " (number->string (cadddr x)) ";
                            break loop;
"))
        (else 
          (string-append id (itemstr- (car x) id true) "(" (implode ", " (map (lambda (y) (itemstr- y "" true)) (cdr x)))
            (if suppress ")" ");\n")))))
    (else (pretty-print x))))


(define (square x)
  (* x x))

(define (sumsquare x)
  (+ (square x) (square x)))

(define (f x)
  (sumsquare x))

(print (f 9))

(define (c x) (lambda (y) x))
(define (a x) (c x))
(define (b x) ((a x) a))

(b b)
((a b) a)
((c b) a)
((lambda (y) b) a)
b

(b b)
((a b) a)
(c b a)
b


for this we need some kind of global args stack 

[]
[b]
call b
[a b]
call a
[a b]
call c

this suggests that we do not want to put things like local variables in there too

[ 12 2 5 3 | 2 4 5 3 | 3 4 | ]

The above shows what it is like to be executing the nested call of a nested call.
So how to pass the argc value into the callee?
Using a separate stack seems a bit like overkill and potentially bad for cache.
We can now either pass the location of the first arg, or the number:

[ 12 2 5 3 |0| 2 4 5 3 |5| 3 4 |10| ]
or
[ 12 2 5 3 |4| 2 4 5 3 |4| 3 4 |2| ]

When we want to do a tail call, we pop off the argc, push more args, and add an updated argc.

We also need both local variables, and return addresses:

[ 12 2 5 3 *0* |0| 2 3 4 / 2 4 5 3 *32* |9| 9 / 3 4 *38* |16| 0 0 / ]

The return address *n* always points to the location where the tree started,

ie.
((foo bar) baz)
 ^ here (n = 0)
or
(foo (bar baz) bar)
     ^ here (n = 23)

It's quite possible that what we actually want is two entry points into the code table,
one is for when we are about to call a function's code,
the other is when we are returning to a label having just called a function's code.

I'm not sure we even need "locals" do we?

If not then we deal with inner calls by evaluating right to left, pushing as we go.

x a b c d e f g = (y (a (b c d)) (e f) g)

note that startcall is always initiated by the parent call
(there may be more args on the stack used in `call y`)

the model is currently confused because i am trying to have it align such that existing stack args
can be left alone and new args fall into place above them ready for the call.

the question then is what to do about local variable storage.
perhaps it would help to have two stacks, one for locals and one for args?

case start_x:

// todo: entry work here, pop off some useful vars and put them in locals area.
// the pertinent information we receive is the start of the args frame and where to return to

  // stack begins with [ prearg1 arg2 nextpc argc=2 ]
  // imagine that x only takes one arg, so the prearg1 is actually for the tail call
  // quickly becomes [ prearg1 *arg3* *arg4* *arg5* nextpc argc=5 ]
  // here nextpc and argc are being thought of as temporary locals.
  // we then perform inner calls in the stack space beyond argc,
  // and mv the return values into either arg3,4,or 5
  argc   = stack[--ebp]
  nextpc = stack[--ebp]
  stack[ebp++] = g
  ebp += 2
  stack[ebp++] = nextpc
  // 1 is number of args used by x itself
  stack[ebp++] = argc + 2 // remove 1 arg, tail call with 3

  stack[ebp++] = f
  stack[ebp++] = finish_e
  stack[ebp++] = 1
  goto e
  continue dispatch
case finish_e:
  stack[ebp-4] = eax
  stack[ebp++] = d
  stack[ebp++] = c
  stack[ebp++] = finish_b
  stack[ebp++] = 2
  goto b
  continue dispatch
case finish_b:
  stack[ebp++] = eax
  stack[ebp++] = finish_a
  stack[ebp++] = 1
  goto a
  continue dispatch
case finish_a:
  stack[ebp-3] = eax
  goto y
  continue dispatch


if the call to y does not use up all args, the engine will keep calling until depleted.

note that there is a difference between pc and x, the return value/current function

do closure arguments have to be copied onto stack or can we reference directly?
if we did reference the closure instead of copy its variables,
we would not be able to take advantage of


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (string-append "                        case " (number->string (car x)) ":\n                            bp -= N;\n"
        (string-append
          (ref-refs (lam-or-clos-arity (car x)) (cadddr tc))
          (p-implode ""
            (map (lambda (x) (itemstr x "                            ")) (tail-call-optimise (car tc) (cadr tc))))
          (if (tmpref? (cadr tc))
            ""
            "")
          (if (varref? (cadr tc))
            (string-append
              (remove-non-return-vars (car x) (cadr tc))
              "                            xc = "
              (itemstr (cadr tc) "")
              ";\n                            break loop;\n")
            ""))))))

(define (generate-lambdas-code)
  (let ((stuff (map lambda-to-code (reverse (table->list internal-lambdas)))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

(define (incref-initials l) "")

(define (preval-to-code x)
(print (cdr x))
(newline)
  (let ((c (cstr-funcall (cdr x) 0 false)))
    (list
      (caddr c)
      (length (cdr (flatten-args (cdr x))))
      (string-append
        (incref-initials (cadddr c))
        (p-implode ""  (map (lambda (x) (itemstr x "                ")) (car c)))))))

(define (generate-prevals-code)
  (let ((stuff (map preval-to-code (filter (lambda (x) (not (lamref? x))) (reverse (table->list internal-prevals))))))
    (list
      (listmax (map car stuff))
      (listmax (map cadr stuff))
      (map caddr stuff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-internal)
  (if (= internal-prevals-id 0)
    ""
    (string-append

(let ((lc (generate-lambdas-code)))
  (string-append
"
/*
 * The smooth object is needed so that external functions can
 * create closures, look up variables on them or call smooth functions
 * passed to them, with smooth.closureApply(fn, [arg1, ...])
 *
 * Note: the object is just a namespace - the following code is valid:
 * var smoothApply = smooth.closureApply;
 * var rv = smoothApply(f, xs);
 *
 * Since minifiers cannot reliably obfuscate an objects properties,
 * the 'closure' object is obfuscated such that:
 *
 * body = {
 *     p: curpos,
 *     n: numlocals,
 *     l: locals
 * }
 *
 * closure = {
 *     c: code,
 *     p: curpos,
 *     b: body
 *     s: scope
 * }
 *
 * These are implementation notes and do not concern the end user,
 * use smooth.closureCreate and smooth.closureLookup for that functionality,
 * as they could change and break your code.
 */
var smooth = (function ()
{
    'use strict';

    var space,
        // small amount of preallocated memory, but could expand past this
        args = new Array(256),
        bp   = 0,
        // used to keep track of the frame sizes
        // we can either take each frame size, or a pointer to the frame end
        // probably a pointer so we can do while (--bp != fp)
        retstack = new Array(128),
        rbp      = 0,
        smoothLambdaSizes = [
            0, "
(generate-arity-values)
"
    ];

//
// Because of the way we enter have arguments on stack we want
// args+0[ args...;locals... | args...;locals...]
//
// A lambda can happily use mem at the top of the stack,
// and just move the bp past their locals when calling an inner function (and back again)
//
// If we were to push args and break out instead of recursively call smoothApply,
// then we would need a way for the callee to make it back to us.
//
// We would break the code over multiple case labels as such:
//

/*

case LAM_1_start:
    push arg1
    push LAM_1_cont_1
    set xc SOME_LAM
    bp += LAM_1_FRAME_SIZE
    break
case LAM_1_cont_1:
    bp -= LAM_1_FRAME_SIZE
    // somehow extract xc, the return value, from the stack...
    ..
    // return to the caller
    jump stack[bp + retaddrOffset]

*/

// The stack frame now looks like: | args... ; returnAddr; locals...; |
//
// I'm mostly concerned that an inner call will lead to something trying to grab all the args!
// It might be necessary to have a secondary stack denoting the number of args that were added on each call


// The case label to return to.
// 0 is special in that it tells us to literally return from Javascript.
// all others are case numbers to jump to.
//    retstack = [ 0, 33, 55, 33 ]

// This knows how many variables are applied at each call.
// when you take a variable, you also decrement the topmost value here, until it is 0,
// at which point you go to the topmost location on the retstack with the value.
// Hence the framestack and retstack always have the same length
//    framestack = [ 4, 6, 8 ]

// For the final call of a given lambda, we don't need to push any frame,
// we just increase the framestack TOS value by the number of args supplied and continue

// using a 'callout area' seems interesting - would it give us any efficiency gains?

//
// [ locals | parameters | return address | saved BP | ? ]
//                                                        - SP
// SP always points to the TOS.
// when making a frame, we push BP, set BP=SP,
// then set SP to the new TOS after the push.
//

    function smoothApply(xc)
    {
        /*
         * The whole of this function represents tight loop code,
         * so every last drop of speed needs to be squeezed out of it.
         *
         * All values are cached in variables
         * (this assumes extra local variables have approx no cost,
         *  not true but should be correct in most or all cases)
         *
         * Non-javascript style formatting to emphasize the difference and
         * make it easier to read the less expressive coding.
         */

        var x, xc, xct, xp, xb, xbp, xbn, xbl, copy, i,

            framestart = retstack[--rbp],

            // if frameret is a >=1, it is the next label to jump to, if 0, return JS
            frameret   = retstack[--rbp],
            framecount = bp - framestart,

            // going into retMode means that an inner function call is going into return,
            // therefore we want to quickly make it back into the smooth code switch
            retnow = false;

        while (true)
        {
            xct = xc.constructor;

            if (!retnow)
            {
                if (xct === Object)
                {
                    // this case means we have a closure,
                    // either an existing args thunk,
                    // or a function referencing a free scope

                    x  = xc;
                    xc = x.c;
                    xb = x.b;

                    if (!xb)
                    {
                        // We have a \"scope closure\" - needs an \"args\" buffer

                        // how many args do we need to fill before call?
                        xbn = smoothLambdaSizes[xc];

                        // work out how far down to copy
                        xp = (framecount >= xbn) ? 0 : (xbn - framecount);
                        framecount = xbn - xp;

                        i   = xbn;
                        xbl = new Array(i);

                        while (i !== xp)
                        {
                            xbl[--i] = args[--bp];
                        }

                        x = {
                            b: {
                                l: xbl,
                                p: xp,
                                n: xbn
                            },
                            s: x.s,
                            c: xc,
                            p: xp
                        };
                    }
                    else
                    {
                        xp  = x.p;
                        xbp = xb.p;

                        // copy on write check against args.body
                        if (xp !== xbp)
                        {
                            xbp = xp;
                            xbn = xb.n;

                            // copy the slice of the body that we need
                            i    = xbn;
                            xbl  = new Array(i);
                            copy = xb.l;

                            while (i-- !== xbp)
                            {
                                xbl[i] = copy[i];
                            }

                            xb = {
                                l: xbl,
                                p: xbp,
                                n: xbn
                            };

                            x.b = xb;
                        }
                        else
                        {
                            xbl = xb.l;
                        }

                        // copy the args into the closure
                        // work out how far down to copy
                        i = (framecount > xp) ? xp : framecount;
                        framecount -= i;

                        while (i--)
                        {
                            xbl[--xp] = args[--bp];
                        }
                        xb.p = xp;

                        // regenerate the head, makes copy on write checks possible
                        x = {
                            b: xb,
                            s: x.s,
                            c: xc,
                            p: xp
                        };
                    }

                    // we need more args to continue
                    if (xp)
                    {
                        if (frameret)
                        {
                            retnow = true;
                            xc = x;
                        }
                        else
                        {
                            return x;
                        }
                    }

                    // we now have all args, so fall through to call the code
                    xct = xc.constructor;
                }
                else
                {
                    x = null;
                }
            }

            if (retnow || (xct === Number))
            {
                xbn = smoothLambdaSizes[xc];

                // if we aren't a closure and lack args, create a thunk
                if (!retnow && !x && (framecount < xbn))
                {
                    // take the args into a closure buffer
                    xbp = xbn;
                    xbl = new Array(xbp);

                    while (bp !== framestart)
                    {
                        xbl[--xbp] = args[--bp];
                    }

                    // note: omit updating framecount since we return here anyway

                    xc = {
                        b: {
                            l: xbl,
                            p: xbp,
                            n: xbn
                        },
                        s: null,
                        c: xc,
                        p: xbp
                    };

                    if (frameret)
                    {
                        retnow = true;
                    }
                    else
                    {
                        return xc;
                    }
                }


/* We need to insert something like the following code for each tail call
   copy stands for 'canTail'.
   ${numapply} is the hardcoded number of args to apply in this call.




*/

                if (retnow)
                {
                    args[bp] = xc; // store the rv
                    xc = frameret;
                    retnow = false;
                }
                else
                {
                    // pop the frame for the lambda we're calling
                    bp -= xbn;
                }

                loop: while (true)
                {
                    switch (xc)
                    {
                        case 0:
                            break;
"
(implode "\n" (caddr lc))
"                    }
                }
            }
            else
            {
                // native call, one arg at a time (or with a closure)
                xc = xc(x ? x : args[--bp]);
                --framecount;
            }

            if (bp === framestart)
            {
                if (frameret)
                {
                    retnow = true;
                }
                else
                {
                    return xc;
                }
            }

            // loop back to apply the remaining args
        }
    }

    space = {

        // Create a closure that runs xc once it has n arguments,
        // initially containing the values held in inp.
        // If inp is empty, xc is returned,
        // which is valid for use in the same places as a closure
        // xc can be a smooth lambda or Javascript function accepting a single argument,
        // but not a closure
        // If the number of values in inp exceeds the number n,
        // the inp values are simply applied directly to xc
        //
        // Note an equivalence:
        // closureCreate(fn, n, args) == closureApply(closureCreate(fn, n, []), args)
        //
        closureCreate:
            function (xc, n, inp)
            {
                var len = inp.length, xp, xbl, i, j;

                // A sanity check to make the API inductive
                if (!n)
                {
                    return xc;
                }

                // Another sanity check
                if (len >= n)
                {
                    return space.closureApply(xc, inp);
                }

                xp  = n - len;
                xbl = new Array(n);
                i = 0;
                j = len;

                // Make a reverse copy of the input
                while (true)
                {
                    if (i >= j)
                    {
                        break;
                    }
                    xbl[xp + i] = inp[--j];
                    xbl[xp + j] = inp[i++];
                }
                return {
                    b: {
                        l: xbl,
                        p: xp,
                        n: n
                    },
                    s: null,
                    c: xc,
                    p: xp
                };
            },

        // Look up the i'th variable in closure x,
        // where i=0 is the most recently applied arg, i=1 the second, etc.
        // It is an error to look up i when it is greater than or equal to
        // the number of arguments that have been applied so far.
        // It is an error to pass anything other than a closure as x.
        closureLookup:
            function (x, i)
            {
                var xb = x.b;
                return xb.l[xb.p + i];
            },

        // Apply the values in the array inp to the smooth function xc,
        // and return the result
        // If inp is an empty array, xc is returned
        // inp may contain as many values as you wish to apply
        // xc can be a Javascript function accepting 1 argument at a time,
        // or a smooth lambda or smooth closure
        closureApply:
            function (xc, inp)
            {
                var n = inp.length, i = 0, j = n;

                // A sanity check to make the API inductive
                if (!j)
                {
                    return xc;
                }

                // Push the args to stack
                while (true)
                {
                    if (i >= j)
                    {
                        break;
                    }
                    args[bp + i] = inp[--j];
                    args[bp + j] = inp[i++];
                }

                retstack[rbp++] = 0;
                retstack[rbp++] = bp;

                bp += n;

                return smoothApply(xc);
            },

        // Runs the smooth code contained here.
        // The entire program is run synchronously.
        // It is an error to call this procedure more than once
        run:
            function ()
            {
"
(let ((p (generate-prevals-code)))
  (string-append
"                var locals = new Array(" (number->string (cadr p)) "),
                    tmp    = new Array(" (number->string (car p)) ");

                delete space.run;

                // this should be a single smoothApply call
"
  (implode "\n" (caddr p))
))
"            }
    };

    return space;

}());

smooth.run();

")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-output-to-js-port h p)
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

  (parse-output-to-js-port (current-output-port)

    (parse-phase-null

      (parse-strip-meta
        (parse-check-errors (current-error-port)
          (parse-input-from-port (current-input-port)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

