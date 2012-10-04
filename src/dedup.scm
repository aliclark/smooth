
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

(define (parse-phase-dedup x) x)

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-dedup
      (read-sexprs-from-port (current-input-port)))))
