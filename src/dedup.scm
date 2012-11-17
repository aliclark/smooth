
(load "base.scm")

;; AFAIK, simple let-style forms, ((fn (varname) ...) (varvalue))
;; should be sufficient to describe any deduplication.

;; The task of deduplication should become simpler if we first
;; do a pass converting all lambdas to use de brujin indexes.

;; The posh term description for all of this is that we
;; are doing alpha-equivalence on each expression with
;; with every other expression, and if they are alpha equivalent,
;; we put in a let-form.
;; The let-form should be evaluating as soon as possible after
;; all the variables its expression needs have been defined.

;; Expressions should always be matched in the fullest amount possible.
;; Of course, larger matched expressions may contain smaller expressions
;; that are also matched, those will be defined in a surrounding let-form.


;; If it is unknown whether the expression will actually be used,
;; ie. all references to the expression are contained within
;; lambda arguments to a native function,
;; then the outer let-form can define a placeholder and a flag,
;; (let ((varname #f) (varname_isset #f)) ...)
;; then at each lambda that needs the expression we have:
;;
;; (if (not varname_isset)
;;   (begin
;;     (set! varname (...the expression...))
;;     (set! varname_isset #t))
;;   'noop)
;;
;; Any lambda expressions within the body of the lambda that performs
;; this setting can itself rely on the value always being set.
;;
;; This is not de-duplication at all, and should possibly be done
;; in a different pass,
;; but the caching of values should make the program quicker.
;; This only applies to evaluations, since lambda references
;; won't have runtime cost.
;;
;; It is probably best to allow the compiler to insert appropriate code
;; to perform this setting operation, since inserting the lambda calculus
;; equivalent would require completely transforming the program
;; to allow it to express side-effects.

;; We basically don't know if a lambda is a closure or not until
;; we have completely traversed the whole object, because
;; any part of the object may refer upwards above that level.

;; We may want to arrange lambdas by how far upwards they reference,
;; starting at 0 for no closed vars, 1 for referring the closest parent
;; var, and so on.

;; this can also apply to expressions, with 0 for expressions made solely
;; of externs, 1 for an expression that references the nearest lambda and so on.

;; For closing expressions, these are equivalent if the refer to the same closures.
;; Or equivalently, if they are alpha-equivalent, and contained by the
;; same lambda expression at the point where their closure starts.
;; --- NOT NECESSARILY TRUE?
;; I would think that actually the *nearest* variable it closes over
;; must be the same, and it will follow that so are the others.
;;
;; Let expressions are not able to take expressions to a higher level,
;; but only if the skipped levels are not referenced by that expression.
;; eg. (fn x (fn y (x x))) can become (fn x (let t (x x) (fn y t)))
;;
;; So ideally we work out the highest possible level that each expression
;; can execute at. Does this include lambdas? Yes.
;;
;; Starting thinking about lambdas from simplest to most complex.
;; The most simple are those that define a single variable.
;;
;; Any expression (lambda or application) that has 0 closedness can and should be moved
;; up to the global level, from where it can be referenced by any other expression.
;;
;; Note that applications should be cacherefs unless the __start__ expression directly uses them
;;
;; We move to a model of a tree of pointers, except where it is invalid to point to
;; expressions which use variables not defined in that context.

;; For each expression, we want to know the highest level at which its
;; value is knowable, and all the places it is used.
;;
;; It is possible to have two expressions of alpha equivalence,
;; which are not the same because the close over different values.
;;
;; So these let-bundles form at the top of each lambda expression,
;; with the magic property being that the expressions in the let bundle
;; all contain at least one variable defined by that lambda.
;;
;; We refer to the let-bindings like __d9__ and so on.
;; We start by completely stripping application expressions down,
;; so that each function,argument pair has it's own let binding,
;; and either of those expressions may themselves be a let binding.

;; The deduplication process is finding out when two __dx__ variables
;; are synonyms for each other.
;; The final step is to arbitrarily pick one as the name, and rename
;; all others to be that name.

;; We start at the leaves, replacing the func,arg pair with a __dx__
;; symbol, then return upwards the tuple:
;; (__dx__, the expression, min closedness count)

;; The min closedness count is the innermost variable that is referred to.
;; Essentially, if you where to propogate outside this limit,
;; then we would be referring to a variable that didn't exist yet!
;; The min closedness count of an expression is the mininimum of the closedness counts
;; of: its constituent __dx__ expressions, and also variables and externs.

;; As we propogate outside lambdas, the closedness count decreases until it is zero,
;; at which point we bind to that lambda.

;; However, we don't just want to deduplicate single applications,
;; and so the process is recursive.

;; (fn f (fn x (f (f (f x)))))
;; becomes
;; (let ((d4 (fn f (let ((d3 (fn x (let ((d0 (f x)) (d1 (f d0)) (d2 (f d1))) d2)))) d3)))) d4)

;; The __dx__ table has exactly one entry per source expression,
;; so we can do some simple global working there.
;;
;; Also note that when we calculate the minimum of min close counts, it's always the
;; same expression that causes the count to be its minimum, so we can follow wherever that is.

;; At the end of this we basically want a valid expression,
;; but where all applications and lambdas are defined by __dx__ variables.
;; and these __dx__ variables are defined as high up as possible.
;;
;; The essential ingredient in all this is the min closedness property


;; Hmm I realise that the inserted let-forms will collide with debrujin
;; notation unless we say they work in different namespaces
;; just have to make sure we don't define any vars with symbol [0-9]+

(define (macropobj x) (parseobj-mk x parseprops-null))

;(define (points-to x)
;  (reserved-symbol-sym? px)

;;; we really want these trees to be doubly linked
;;; so then they can be built from the leaves up,
;;; but also accessed that way.

;(define (build-trees vs trees)
;  (if (null? vs)
;    trees
;    (let* ((a (car vs))
;           (pt (points-to a)))
;      

(define (unified-equal? px py defs)
  (let ((x (parseobj-obj px))
         (y (parseobj-obj py)))

    (if (and (list? x) (list? y))

      (if (reserved-form-type? px '__lambda__ 3)
        (if (reserved-form-type? py '__lambda__ 3)

          (if (eq? (parseobj-obj (cadr x)) '__debrujin__)
            (if (eq? (parseobj-obj (cadr y)) '__debrujin__)
              (unified-equal? (caddr x) (caddr y) defs)
              #f)

            (if (eq? (parseobj-obj (cadr x)) (parseobj-obj (cadr y)))
              (unified-equal? (caddr x) (caddr y) defs)
              (unified-equal? (caddr x) (caddr y) (assoc-set defs (parseobj-obj (cadr x)) (parseobj-obj (cadr y))))))

          #f)

        (if (reserved-form-type? px '__extern__ 2)
          (equal? (parse-strip-inner px) (parse-strip-inner py))

          (if (reserved-symbol-obj? px)

            ;;; FIXME: ugly !!!! chnage to proper gensyms.
            (if (string=? (substring (symbol->string (parseobj-obj (car x))) 0 3) "__d")
              (and (= (length x) (length y))
                (unified-equal? (car x) (car y) defs)
                (unified-equal? (cadr x) (cadr y) defs))

              (equal? (parse-strip-inner px) (parse-strip-inner py)))

            (and (= (length x) (length y))
              (unified-equal? (car x) (car y) defs)
              (unified-equal? (cadr x) (cadr y) defs)))))

      (if (and (symbol? x) (symbol? y))

        (if (reserved-symbol-sym? px)

          ;;; FIXME: ugly !!!! change to proper gensyms.
          (if (string=? (substring (symbol->string x) 0 3) "__d")

            (or (eq? x y) (eq? (assoc-ref defs x '()) y))

            (eq? x y))

          (eq? x y))

        #f))))

(define (group-expressions vs)
  (if (null? vs)
    '()
    (let* ((a (car vs))
            (aexp (cadr a))
            (rvs (cdr vs))
            (parvs (part (lambda (x) (unified-equal? aexp (cadr x) '())) rvs)))
      (cons (cons a (car parvs)) (group-expressions (cadr parvs))))))

; replace all occurrences of vs in exp with arg,
; except those shadowed by a lambda
(define (subst-all exp vs arg)
  (let ((x (parseobj-obj exp)))
    (if (list? x)
      (if (reserved-form-type? exp '__extern__ 2)
        exp
        (if (reserved-form-type? exp '__lambda__ 3)
          (let ((s (parseobj-obj (cadr x))))
            (if (contains? vs s)
              exp
              (parseobj-sel 2 (lambda (b) (subst-all b vs arg)) exp)))
          (parseobj-mk (map (lambda (y) (subst-all y vs arg)) x)
            (parseobj-propsid exp))))
      (if (contains? vs x)
        arg
        exp))))

;; now for any groups that have more than 1 member,
;; we rename all but the head vars to be the same as the head
;; apply this renaming in all of the rest of the groups,
;; and in the main body expression itself
;; (though i think it should be the case that the expression only appears once)
(define (perform-renames gs exp)
  (if (null? gs)
    (list '() exp)
    (let ((stuff (perform-renames (cdr gs) exp)))
      (let ((g (car gs)))
        (if (= (length g) 1)
          (list (cons (car g) (car stuff)) (cadr stuff))

          (let ((to (caar g))
                 (renames (map car (cdr g))))
            (list
              (cons (car g)
                (map
                  (lambda (v)
                    (list (car v)
                      (subst-all (cadr v) renames (macropobj to))
                      (caddr v)
                      (cadddr v)))
                  (car stuff)))
              (foldr (lambda (x acc) (subst acc x (macropobj to))) (cadr stuff) renames))))))))

(define (make-letex vs exp)
  (if (null? vs)
    exp

    (if (and (= (length vs) 1) (eq? (caar vs) (parseobj-obj exp)))
      (cadar vs)

      (let ((va (car vs)))
        (macropobj
          (list
            (macropobj
              (list (macropobj '__lambda__) (macropobj (car va))
                (make-letex (cdr vs) exp)))
            (cadr va)))))))

;; This is efficient enough for the global let-form of cat.smc,
;; which has 133 expressions initially.
;; It is not efficient enough for the global let-form of comments.smc,
;; which has 2802 expressions initially.

;; The vs must be in an order such that each item does not depend on
;; the definition of a following item.
(define (make-letex-deduping vs exp)

  ;; lets do the deduplication here instead of in another pass
  ;; That means we want to check if any vs are the same
  ;; in terms of alpha equivalence

  ;; __v1__ is alpha equivalent to __v2__ if the expressions
  ;; they refer to are alpha equivalent

  ;; we can build sort of a tree of which expressions contain which.
  ;; the truth we want to exploit is that a parent expression
  ;; can never be equal to its child.
  ;; so once we have created our tree, equality can only occur
  ;; with siblings or distant relations
  ;; An element is never equivalent to its parent* or child*
  ;; but can be equivalent to anything else.
  ;; Even in that case, the tree must be the same shape.

  ;; The ideal is to match largest objects possible.
  ;; We should still be matching from the leaves first,
  ;; just being as greedy as possible.
  ;; >
  ;; Actually, it shouldn't matter too much if we first
  ;; make equivalence with a couple of leaves and then
  ;; make equivalence with their parents.
  ;; It's probably easier, because if 3 branches start matching,
  ;; one might stop matching earlier, so this needs to be handled.

  ;; Starting with all the leaf elements, we group by equality.
  ;; we can immediately discard those which do not match,
  ;; as well as the entirety of the path to the root node
  ;; (but not other branches of that root node).

  ;; After deduplicating, we want to replace any lone appearances of a
  ;; variable with the value itself, and then after that look at doing
  ;; a conditional-use cache

  ;; for each variable in the let form we work out if it has been
  ;; renamed and if so to what.
  ;; if it has been renamed then we need to update all parts of the code.
  ;; the one which was defined earliest is the winner, because that has
  ;; most potential to be used in snippets of code.

  ;; To begin with we only consider (E E) and (lambda debrujin E)
  ;; where E is an extern or variable lookup.

  (if (null? vs)
    exp

    (if (and (= (length vs) 1) (eq? (caar vs) (parseobj-obj exp)))
      (cadar vs)

      (let ((gs (group-expressions vs)))
        (let* ((vsexp (perform-renames gs exp))
                (vs (car vsexp))
                (exp (cadr vsexp)))

          (make-letex vs exp))))))

(define (reassemble-cond px flist fsym)
  (let ((x (parseobj-obj px)))
    (parseobj-mk 
      (if (symbol? x)
        (fsym x)
        (apply flist x))
      (parseobj-propsid px))))

(define (reassemble px f) (reassemble-cond px f f))

;; Any debrujin references to closure variables are decremented
(define (decrement-closing px depth)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        (if (eq? (parseobj-obj (cadr x)) '__debrujin__)
          (parseobj-sel 2
            (lambda (exp)
              (decrement-closing exp (+ depth 1)))
            px)

          (parseobj-sel 2
            (lambda (exp)
              (decrement-closing exp depth))
            px))

        (if (reserved-form-type? px '__extern__ 2)
          px

          (if (reserved-symbol-obj? px)
            px

            (parseobj-map
              (lambda (y)
                (decrement-closing y depth))
              px))))

      (if (reserved-symbol-sym? px)
        px
        (let ((n (symbol->number x)))
          (if (> n depth)
            (parseobj-mk (number->symbol (- n 1)) (parseobj-propsid px))
            px))))))

(define (upward v) (if (number? v) (if (= v 0) 0 (- v 1)) #f))

(define (upward-globalising v) (if (and (number? v) (= v 0)) #f (upward v)))

(define (upwards vs)
  (map
    (lambda (x)
      (list (car x)
        (decrement-closing (cadr x) 0)
        (upward (caddr  x))
        (map upward (cadddr x))))
    vs))

(define (part f xs)
  (if (null? xs)
    (list (list) (list))
    (let ((a (car xs))
           (r (part f (cdr xs))))
      (if (f a)
        (list (cons a (car r)) (cadr r))
        (list (car r) (cons a (cadr r)))))))

(define (local-usage? x)
  (or
    (and (number? (caddr  x)) (= (caddr  x) 0))
    (contains? (cadddr x) 0)))

(define (symbol->number x) (string->number (symbol->string x)))

(define counter 0)
(define (mygensym)
  (let ((c counter))
    (set! counter (+ counter 1))
    (string->symbol (string-append "__d" (number->string c) "__"))))

(define (minrefcount x y)
  (if (eq? x #f)
    y
    (if (eq? y #f)
      x
      (min x y))))

(define (merge-lists x y)
  (if (null? x)
    y
    (merge-lists (cdr x) (ensure-contains (car x) y))))

;; Returns a list containing:
;; 1: the modified expression,
;; 2: a list of expressions to propogate: (list x) x=(list __dx__ expression minclosedness)
;; 3: the minrefcount  of this expression (the absolute closest bound variable)
;; 4: list of the mincloscounts of this expression (the closest bound variable that is not locally bound, #f otherwise)
(define (dedup-extract px)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (reserved-form-type? px '__lambda__ 3)
        (let* ((e (dedup-extract (caddr x)))
                (vs (part local-usage? (cadr e)))
                (sym (mygensym))
                (minref  (upward-globalising (caddr e)))
                (minclos (map upward-globalising (cadddr e))))

          (list
            (macropobj sym)

            (cons
              (list sym
                (macropobj
                  (list (macropobj '__lambda__) (cadr x)
                    ;; XXX: we don't really need to do a full reverse
                    ;; because expressions at different lambda levels
                    ;; cannot possibly reference each other
                    (make-letex-deduping (reverse (car vs)) (car e))))
                minref
                minclos)
              (upwards (cadr vs)))

            minref
            minclos))

        (if (reserved-form-type? px '__extern__ 2)
          (list px (list) #f (list #f))

          (if (reserved-symbol-obj? px)
            ;; currently we don't allow expressions to move up
            ;; if we don't know what they do
            (list px (list) 0 (list 0))

            (let* ((e1 (dedup-extract (car x)))
                   (e2 (dedup-extract (cadr x)))
                   (sym (mygensym))
                   (minref  (minrefcount  (caddr  e1) (caddr  e2)))
                   (minclos (merge-lists (cadddr e1) (cadddr e2))))

              (list (macropobj sym)
                (cons (list sym (macropobj (list (car e1) (car e2))) minref minclos) (append (cadr e1) (cadr e2)))
                minref
                minclos)))))

      (list px (list) (symbol->number x) (list (symbol->number x))))))

(define parse-phase-dedup
  (parseobj-convf
    (lambda (xs)
      (map
        (lambda (px)
          (if (reserved-form-type? px '__start__ 2)
            (parseobj-sel 1
              (lambda (x)
                (let ((e (dedup-extract x)))
                  ;; at this stage, all maxcounts should be #f
                  (make-letex-deduping (reverse (cadr e)) (car e))))
              px)
            px))
        xs))))

(define (start)
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (parse-phase-dedup (cadr pxl)))))