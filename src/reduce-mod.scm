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

;;; This phase takes a single lambda calc expression, possibly containing
;;; primops, and reduces the lambda calculus expressions as much as possible.
;;;
;;; Strategy is call by need, so we paste in definitions to arguments
;;; without evaluating them, and only later do we reduce the actual arguments.
;;;
;;; (lambda x ((lambda y y) (lambda z z)))
;;;
;;; is be reduced to
;;;
;;; (lambda x (lambda z z))
;;;
;;; Some terms are not reducible (lambda calculus is Turing equivalent)
;;;
;;; ((lambda x (x x)) (lambda y (y y)))
;;;
;;; in this example the recursion should be detected on the third step.
;;; A simple example of a recursive function on a primop value not known until run-time
;;;
;;; if    = (lambda x x)
;;; true  = (lambda a (lambda b a))
;;; false = (lambda a (lambda b b))
;;; --    = (lambda n (lambda f (lambda x (n (lambda g (lambda h (h (g f)))) (lambda u x) (lambda u u)))))
;;; zero? = (lambda n (n (lambda x false) true))
;;; *     = (lambda m (lambda n (lambda f (m (n f)))))
;;; g     = (lambda r (lambda n (if (zero? n) 1 (* n (r r (-- n))))))
;;; f     = (g g UINT3)
;;;
;;; > f
;;; -> (g g UNIT3)
;;; -> (if (zero? UINT3) 1 (* UINT3 (g g (-- UINT3))))
;;; -> ((zero? UINT3) 1 (* UINT3 (g g (-- UINT3))))
;;; -> ((UINT3 (lambda x false) true) 1 (* UINT3 (g g (-- UINT3))))
;;;
;;; At this point the recursion can stop because the head of the expression is a primop.
;;; Similarly, the other nodes (* UINT3 ...) and (-- UINT3) should expand but at some point
;;; stop due to applying UINT3 as a function.
;;;
;;; Still not sure what logic is used to ensure (g g (-- UINT3)) does not recurse.
;;; Perhaps we could use the rule that for some (r r (-- n)) which we have already
;;; entered into execution of, we cannot enter into another (r r (-- n)),
;;; unless all arguments are known at current time.
;;;
;;; > f
;;; -> (((lambda r1 (lambda n (if (zero? n) 1 (* n (r1 r1 (-- n)))))) (lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n))))))) UNIT3)
;;; [r1 = (lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n))))))]
;;; -> ((lambda n (if (zero? n) 1 (* n ((lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n)))))) (lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n)))))) (-- n))))) UNIT3)
;;; [r2 = (lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n))))))]
;;; -> ((lambda n (if (zero? n) 1
;;;      (* n ((lambda n (if (zero? n) 1
;;;        (* n ((lambda r2 (lambda n (if (zero? n) 1
;;;          (* n (r2 r2 (-- n))))))
;;;          (lambda r2 (lambda n (if (zero? n) 1 (* n (r2 r2 (-- n)))))) (-- n))))) (-- n))))) UNIT3)
;;;
;;; The above method of recursion detection keeps track of each variable assignment
;;; by storing them in a table.
;;; Assuming we are not renaming variables on lambdas when they are assigned to a variable,
;;; at some point we will recurse into an assignment where the variable is assigned to the same value as before.
;;; At this point we will know that (r2 r2) at some point calls for (r2 r2) so it is not safe to do that indefinitely.
;;; Recursion is okay if we know all the information, but in this case we don't, because in (-- n) n is bound to a primop.
;;;
;;; First keep applying arguments into the head until you can no longer do so.
;;; at this point, we have a primop at the head, or an infinite loop.
;;; Unless we have reached a primop head, we do not need to show caution.
;;;
;;; If we have reached a primop head, we can still reduce it's arguments,
;;; but we must remember the configuration of the previous stoppages.
;;; In particular, we know that the stoppage was caused by a certain primop symbol.
;;; We don't know much more about that symbol though, because future stoppages might use it differently.
;;;
;;; Naturally, we repeat this process across the arguments to the primop.
;;; If at some point we are asked to make a binding already made earlier in this execution,
;;;
;;; Assuming all lambdas in the file have different variable names, we
;;; can make a note at each stoppage what the most recently bound variable name is.
;;; Then if we find a new stoppage evaluating the arguments,
;;; we can look up the list of variables bound since last stoppage and compare to remembered stoppages,
;;; then only evaluate the arguments if there is no match.

;;; It may be a useful exercise to first pass through the code sending out any lambdas
;;; with no free variables, since this should reduce the complexity of the actual
;;; reduction by limiting the number of variables in the scope of a given expression.
;;; This could be done at the same time as a gensyming pass.

;;; After the send-out-lambdas phase in c-mod we have everything in the desired format,
;;; with unique lambda references.

;;; Before reductions, there will be exactly one reference to each lambda,
;;; however, after reductions this will change as some lambdas are applied
;;; and others are copied by multiple variable references.

;;; What I really want is to detect a stoppage in the same positional location
;;; as a lambda evaluation up the execution chain.
;;;
;;; z = (lambda v (lambda x (v x)))
;;;
;;; In the above example, if v stops and x executes z, which stops on something else,
;;; then we know to not evaluate that inner x.

;;; For the copied lambda references we can use another table called internal-copies,
;;; with copyid => lamref

((lambda x (a b c x)) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ensure-contains cons)

(define (contains? l x)
  (if (null? l) false (if (equal? (head l) x) true (contains? (tail l) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main-symbol 'main)

(define internal-lambdas (make-table))

(define (reduce-value p)
  (cond
    ((lambda-expression? p)
      (lambda-expression-mk (lambda-expression-var p)
        (reduce-value (ensure-contains shadows (lambda-expression-var p))
          (lambda-expression-body p))))
    ((non-resvd-list? p) (p-map (partial reduce-value shadows) p))
    ((p-symbol? p)
      (if (contains? shadows p) p
        (reduce-value p-null (table-ref internal-defines p))))
    (else p)))

(define (parse-phase-filter xs) (p-list (reduce-value (p-first xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)

  (parse-output-to-file (current-output-port)

    (parse-phase-filter

      (parse-strip-meta
        (parse-check-errors (current-error-port)
          (parse-input-from-file (current-input-port)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init

(start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

