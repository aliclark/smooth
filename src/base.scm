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

; This is the ULC to C print module.
; It comprises the final small part of the compiler pipeline,
; where the fully refined lambda language is finally converted into C code.

; Compiler pipeline
;
; * read macro expansion
; * macro expansion
; * lambda expression applications/reductions
; * filter for just the main lambda expression
; * ULC to C print

; Interpreter pipeline
;
; - Input into interpreter
; | * read macro expansion
; | * limited macro expansion
; - If the input is a definition/defmacro/defreadmacro,
;   - Remember it in table and read next.
;   - else the form is an expression, do the following
;   | * macro expansion
;   | * lambda expression applications/reductions
;   | * run interpreted lambda code
;
; Limited macro expansion expands the outmost form until it is found to be
; either an expression or a definition/defmacro/defreadmacro form.

; Module pipeline structure
;
;         PARSE          PASS           PRINT
; outputC  ->  internalC  ->  internalB  ->  outputB
;                 V
; outputB  ->  internalB  ->  internalA  ->  outputA
;                 V
; outputA  ->  internalA  ->  internalA  ->  C code
;
; All modules are able to use the exact same print operation except for the final
; translation module.
; All modules are able to use the exact same parse operation except for the first
; reader module, which would use a wrapper procedure around the standard parser,
; allowing it to read additional syntax.
; The pass phase of both the first and last modules should be null, as any
; intermediate work on parse trees should be done in intermediate phases only.

; Examples of internal symbols used by any of the modules: [a-zA-Z0-9]
; (()lambda) (()define) (()delay) (()force) (()begin) (()gensym 200) (()primop smoothlang anc2020 iochar cfgetc)
;
; If an internal symbol is found at the head of a top-level form and it is not understood, eg. ((()gibberish) ((()lambda) x x))
; then it causes that entire form to be ignored (possibly remembered to pass onto next stage at the discretion of the module that sees it).
; If such an unrecognised form is a child of a recognised form, an error is thrown.
;
; There is a difference between Smooth's approach to internal symbols and the approach of other lisps
; that in other lisps, internal symbols occupy the same format as user symbols, eg. (lambda (x) x)
;
; In that case, we can also do (lambda (lambda) lambda) and it should be valid syntax,
; with the second and third 'lambda' being user symbols.
;
; But with Smooth, there is a syntax error if internal symbols are used in the wrong place.
; ((()lambda) ((()lambda)) (()lambda))

(load "gambcini.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library code

(define false #f)
(define true  #t)

(define tail cdr)
(define head car)

(define (compose f g) (lambda (x) (f (g x))))

(define (implode- acc glue l)
  (if (null? l) acc
    (implode-
      (string-append acc
        (if (null? (tail l)) (head l) (string-append (head l) glue)))
      glue
      (tail l))))

(define (implode glue l) (implode- "" glue l))

(define (filter f xs) (if (null? xs) '() (if (f (car xs)) (cons (car xs) (filter f (cdr xs))) (filter f (cdr xs)))))

(define (list-contains? xs x)
  (if (null? xs)
    #f
    (if (eq? (car xs) x)
      #t
      (list-contains? (cdr xs) x))))

(define (take-first f xs nf)
  (if (null? xs)
    nf
    (let ((c (car xs)))
      (if (f c) c (take-first f (cdr xs) nf)))))

(define (part f xs)
  (if (null? xs)
    (list (list) (list))
    (let ((a (car xs))
           (r (part f (cdr xs))))
      (if (f a)
        (list (cons a (car r)) (cadr r))
        (list (car r) (cons a (cadr r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Domain specific library code

;;; p-symbol and p-list are th

(define p-symbol?  symbol?)
(define p-symbol-string symbol->string)

(define p-list?  list?)
(define p-null?  null?)
(define p-null   (list))
(define p-length length)
(define p-cons   cons)
(define p-rest   cdr)
(define p-first  car)
(define p-second cadr)
(define p-third caddr)
(define p-list list)

(define (p-last l)  (if (p-null? (p-rest l)) (p-first l) (p-last (p-rest l))))
(define p-map map)
(define p-reverse reverse)
(define p-implode implode)
(define p-append append)

;; This takes a p-list of char and converts it to p-symbol
(define (p-list-to-p-symbol x) (string->symbol (list->string x)))

;;; parseobj is the intermediate format for code throughout the whole pipeline.
;;; ls is line-start, ce is column-end
;;; val may be either a p-symbol or a p-list of zero or more parseobj
;;;
;;; A symbol of one letter starting at the top of the file would have
;;; (parseobj-mk 0 0 'a 0 1)
;;; An empty list starting at the top of the file would have
;;; (parseobj-mk 0 0 '() 0 2)

(define (parseobj-mk ls cs val le ce) (list ls cs val le ce))
(define parseobj-ls   car)
(define parseobj-cs   cadr)
(define parseobj-val  (compose car  cddr))
(define parseobj-le   (compose car  cdddr))
(define parseobj-ce   (compose cadr cdddr))
(define parseobj-null  (list))
(define parseobj-null? null?)

(define (reserved-symbol-mk s . v) (append (p-list p-null s) v))
(define reserved-symbol-type cadr)
(define reserved-symbol-val  caddr)
(define (reserved-symbol? x) (and (p-list? x) (not (p-null? x)) (p-null? (p-first x))))
(define (reserved-symbol-type? x t) (and (reserved-symbol? x) (eq? (reserved-symbol-type x) t)))
(define (non-resvd-list? x) (and (p-list? x) (not (reserved-symbol? x))))

(define (reserved-symbol-sym? px)
  (let ((x (parseobj-obj px)))
    (if (symbol? x)
      (let* ((sx (symbol->string x))
              (len (string-length sx)))
        (and (> len 4)
          (string=? (substring sx 0 2) "__")
          (string=? (substring sx (- len 2) len) "__")))
      #f)))

(define (reserved-symbol-obj? py)
  (let ((y (parseobj-obj py)))
    (if (and (list? y) (> (length y) 0))
      (let* ((px (car y))
             (x (parseobj-obj px)))
        (if (symbol? x)
          (let* ((sx (symbol->string x))
                  (len (string-length sx)))
            (and (> len 4)
              (string=? (substring sx 0 2) "__")
              (string=? (substring sx (- len 2) len) "__")))
          #f))
      #f)))

(define (reserved-form-type? x t)
  (and (p-list? x) (not (p-null? x)) (reserved-symbol? (p-first x))
    (eq? (reserved-symbol-type (p-first x)) t)))

(define (primop-symbol? x) (reserved-symbol-type? x 'primop))
(define (primop-arity x) (string->number (symbol->string (cadr (reserved-symbol-val x)))))
(define (primop-to-id x) (symbol->string (car (reserved-symbol-val x))))

(define (resvd-lambda? x) (reserved-symbol-type? x 'lambda))

(define (lambda-expression? x) (reserved-form-type? x 'lambda))
(define (lambda-expression-mk v b) (p-list (reserved-symbol-mk 'lambda) v b))
(define lambda-expression-var cadr)
(define lambda-expression-body caddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parsing
;;;
;;; * ( and ) start / end a list
;;; * Whitespace (spaces, tabs and newlines) seperates items
;;;
;;; That's it. The first module in the pipeline may want to provide a wrapper
;;; around this read function so that it can understand other syntax.

(define cspace   #\space)
(define cnewline #\newline)
(define ctab     #\tab)
(define coparen  #\()
(define ccparen  #\))

(define (parse-white-space? c)
  (and (not (eof-object? c))
    (or (char=? c cspace) (or (char=? c cnewline) (char=? c ctab)))))

(define (parse-open-paren?  c) (and (not (eof-object? c)) (char=? c coparen)))
(define (parse-close-paren? c) (and (not (eof-object? c)) (char=? c ccparen)))
(define (parse-paren?       c) (or (parse-open-paren? c) (parse-close-paren? c)))

(define (parse-error s)
  (display s (current-error-port))
  (newline (current-error-port))
  (exit))

(define (parse-strip-spaces ls cs p)
  ((lambda (c)
    (if (parse-white-space? c)
      (begin (read-char p) (parse-strip-spaces ls cs p))
      false))
    (peek-char p)))

(define (parse-reads ls cs p s)
  ((lambda (c)
    (if (or (parse-white-space? c) (parse-paren? c))
      (p-list-to-p-symbol (p-reverse s))
      (begin (read-char p) (parse-reads ls cs p (p-cons c s)))))
    (peek-char p)))

(define (parse-read-symbol ls cs p)
  (parseobj-mk ls cs (parse-reads ls cs p (p-cons (read-char p) p-null)) ls cs))

(define (parse-readl ls cs p l)
  (begin
    (parse-strip-spaces ls cs p)
    ((lambda (c)
      (if (eof-object? c)
        (parse-error
          (string-append "Error: Incomplete list (list started on line "
            (number->string ls)
            ", column "
            (number->string cs)
            ")"))
        (if (parse-open-paren? c)
          (parse-readl ls cs p (p-cons (parse-read-list ls cs p) l))
          (if (parse-close-paren? c) (begin (read-char p) (p-reverse l))
            (parse-readl ls cs p (p-cons (parse-read-symbol ls cs p) l))))))
      (peek-char p))))

(define (parse-read-list ls cs p)
  (begin (read-char p) (parseobj-mk ls cs (parse-readl ls cs p p-null) ls cs)))

(define (parse-next ls cs p)
  (begin
    (parse-strip-spaces ls cs p)
    ((lambda (c)
      (if (eof-object?  c) parseobj-null
        (if (parse-open-paren?  c) (parse-read-list ls cs p)
          (if (parse-close-paren? c)
            (parse-error
              (string-append
                "Error: List ending while not in a list (found at line  "
                (number->string ls)
                ", column "
                (number->string cs)
                ")"))
            (parse-read-symbol ls cs p)))))
      (peek-char p))))

(define (parse-port-helper ls cs p)
  (let ((pn (parse-next ls cs p)))
    (if (parseobj-null? pn)
      p-null
      (p-cons pn (parse-port-helper ls cs p)))))

;; This takes a file handle and returns the objects within,
;; exiting if it finds a syntax error.
(define (parse-input-from-port p)
  (let ((pp (parse-port-helper 0 0 p)))
    (if (p-null? pp)
      (parseobj-mk 0 0 pp 0 0)
      (parseobj-mk 0 0 pp (parseobj-le (p-last pp)) (parseobj-ce (p-last pp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This gives the start and length of each object
;; in the original input stream
;; Together with the file of origin we find see the lines involved.
;;
;; TODO: make parseobj a sort of assoc-list of properties
;; whereby the source-offset and source-length properties
;; are automatically filled in,
;; and the source-file property can be filled in by "load" pass.
;;
;; TODO: need to read parseobjects with offset and length
;; as integers, not symbols
;; - however, since we can't guarantee type conversion for all properties
;;  (or cba) may as well leave it as symbol?

(define (number->symbol x) (string->symbol (number->string x)))

(define (assoc-set l k v)
  (if (null? l)
    (list (list k v))
    (let ((c (car l)))
      (if (eq? (car c) k)
        (cons (list k v) (cdr l))
        (cons c (assoc-set (cdr l) k v))))))

(define (assoc-ref l k nf)
  (if (null? l)
    nf
    (let ((c (car l)))
      (if (eq? (car c) k)
        (cadr c)
        (assoc-ref (cdr l) k nf)))))

(define (parseobj-mk obj props) (list obj props))

(define (parseobj-obj     p) (car p))
(define (parseobj-propsid p) (cadr p))

(define (parseobj-props propstab p)
  (if (= (parseobj-propsid p) parseprops-null)
    '()
    (assoc-ref propstab (parseobj-propsid p) #f)))

(define (parseobj-set propstab p k v)
  (if (not (symbol? k))
    (begin (display 'bad-parseobj-key-type) '()) ; there is a problem
    (parseprops-set propstab (parseobj-propsid p) (assoc-set (parseobj-props propstab p) k v))))

(define (parseobj-ref propstab p k nf)
  (assoc-ref (parseobj-props propstab p) k nf))

(define (parseobj-src-start  p) (parseobj-ref p 'source-start  #f))
(define (parseobj-src-length p) (parseobj-ref p 'source-length #f))

(define (parseprops-set propstab id p)
  (assoc-set propstab id p))

;; FIXME: this will reset the counter to 1 between program invocations
;; - something we absolutely don't want.
(define parseprops-null 0)
(define parseobj-counter 1)
(define (parseobj-freshid!)
  (let ((c parseobj-counter))
    (set! parseobj-counter (+ parseobj-counter 1))
    c))

(define (parseprops-create s l)
  (assoc-set
    (assoc-set '() 'source-start (number->symbol s))
    'source-length (number->symbol l)))

(define (parseobj-conv  lam x) (parseobj-mk (lam (parseobj-obj x)) (parseobj-propsid x)))
(define (parseobj-convf lam) (lambda (x) (parseobj-conv lam x)))

;;; TODO:
;;; A next step would be to automatically assign each object
;;; a unique properties entry given its position in the program (eg. left to right, back to front)
;;; A list of "null" entries can be maintained where we don't have any info
;;; for that object.
;;;
;;; This allows us to pass the intermediary program code with just a single extra properties table
;;; and no goop cluttering up the code!
;;; The parseobj's are reconstructed and deconstructed at read and write time, with O(n) cost.

(define (parseobj-propstable py)
  (cadar
    (take-first
      (lambda (px) (reserved-form-type? px '__propstab__ 2))
      (parseobj-obj py)
      '(((__propstab__ 0) ()) 0))))

;; for each entry in b, generate a fresh entry for use in a
(define (propstable-remap a b)
  (let loop ((rv '()) (remb b))
    (if (null? remb)
      rv
      (loop (cons (list (caar remb) (parseobj-freshid!)) rv) (cdr remb)))))

(define (propstable-add a b rmap)
  (let loop ((rv a) (remmap rmap))
    (if (null? remmap)
      rv
      (loop (assoc-set rv (caar remmap) (assoc-ref b (caar remmap) #f)) (cdr remmap)))))

(define (parseobj-sel-inner i lam x)
  (if (= i 0)
    (cons (lam (car x)) (cdr x))
    (cons (car x) (parseobj-sel-inner (- i 1) lam (cdr x)))))

(define (parseobj-sel i lam px)
  (let ((x (parseobj-obj px)))
    (if (list? x)
      (if (<= (length x) i)
        '() ; error
        (parseobj-mk (parseobj-sel-inner i lam x) (parseobj-propsid px))))))

(define (parseobj-map lam px)
  (let ((x (parseobj-obj px)))
    (parseobj-mk (map lam x) (parseobj-propsid px))))

(define (parseobj-lambda? px)
  (let ((x (parseobj-obj px)))
    (and (list? x)
      (= (length? x) 3)
      (let ((items (map parseobj-obj x)))
        (and (null? (car items))
          (symbol? (cadr items))
          ; todo: some sensible expression checking
          )))))

(define (parseobj-decspecial? px)
  (let ((x (parseobj-obj px)))
    (and (list? x)
      (>= (length? x) 3)
      (let ((items (map parseobj-obj x)))
        (and (null? (car items))
          (symbol? (cadr items))
          (null? (p-last items)))))))

(define (sexpr-decspecial? x)
  (and (list? x) (>= (length? x) 3)
    (null? (car x)) (symbol? (cadr x)) (null? (p-last x))))

;; Returns a list of all decspecial symbols in this object's top level
;; if a decspecial is defined multiple times this will have multiple
;; entries for the decspecial
(define (parseobj-decspecials px)
  (let ((xs (parse-strip-meta px)))
    (if (not (list? xs))
      '()
      (let loop ((rem xs) (rv '()))
        (if (null? rem)
          (reverse rv)
          (if (sexpr-decspecial? (car rem))
            (loop (cdr rem) (cons (sexpr-decspecial-name (car rem)) rv))
            (loop (cdr rem) rv)))))))

;; given a root object and symbol, is that symbol defined as a decspecial?
(define (parseobj-has-decspecial? px d) (list-contains? (parseobj-decspecials px) d))

;; If parseobj is a non-empty list, the head of that list
(define (parseobj-head px nf)
  (let ((o (parseobj-obj px)))
    (if (and (list? o) (>= (length o) 1))
      (car o)
      nf)))

(define (parseobj-has-head px d) (eq? (parseobj-has-head ps #f) d))

;; list of all root objects which have a special at their head
(define (parseobj-root-specials px)
  (let ((obj (parseobj-obj px)))
    (if (not (list? obj))
      '()
      (let ((sl (parseobj-decspecials px)))
        (filter
          (lambda (x)
            (list-contains? sl (parseobj-obj (parseobj-head x))))
          obj)))))

(define ensure-contains cons)

(define (contains? l x)
  (if (null? l) false (if (equal? (head l) x) true (contains? (tail l) x))))

(define (reserved-form-type? px s ln)
  (let ((x (parseobj-obj px)))
    (and (list? x) (or (= ln 0) (= (length x) ln)) (eq? (parseobj-obj (car x)) s))))

(define (lambda-expression? x) (reserved-form-type? x '__lambda__ 3))
(define (lambda-expression-var x) (cadr (cadr x)))
(define (lambda-expression-body x) (caddr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-whitespace? c)
  (or (char=? c cspace) (or (char=? c cnewline) (char=? c ctab))))

;; (list (list parseobj remcl len) propstab)
(define (parse-read-list cl startn propstabin)
  ; (cdr cl) moves past the open paren
  (let loop ((remcl (cdr cl)) (len 1) (rv '()) (propstab propstabin))
    (if (null? remcl)
      '() ; this is an error - no autoclosing

      (let ((firstl (first-nonwhitespace-ch remcl)))

        (if (null? firstl)
          '() ; this really shouldnt happen

          (let* ((fcl (car firstl))
                 (fln (cadr firstl))
                 (lnsofar (+ len fln)))

          (if (parse-close-paren? (caar firstl))
            (let ((id (parseobj-freshid!)))
              (list
                (list (parseobj-mk (reverse rv) id) (cdr fcl) (+ lnsofar 1))
                (parseprops-set propstab id (parseprops-create startn (+ lnsofar 1)))))

            (let* ((readf (if (parse-open-paren? (car fcl)) parse-read-list parse-read-symbol))
                    (plx (readf fcl (+ startn lnsofar) propstab))
                    (pl (car plx)))
                (loop (cadr pl) (+ lnsofar (caddr pl)) (cons (car pl) rv) (cadr plx))))))))))

;; (list (list parseobj remcl len) propstab)
(define (parse-read-symbol cl startn propstab)
  (let loop ((remcl cl) (n 0) (rv '()))
    (if (or (null? remcl) (parse-white-space? (car remcl)) (parse-paren? (car remcl)))
      (let ((id (parseobj-freshid!)))
        (list
          (list (parseobj-mk (p-list-to-p-symbol (reverse rv)) id) remcl n)
          (parseprops-set propstab id (parseprops-create startn n))))
      (loop (cdr remcl) (+ n 1) (cons (car remcl) rv)))))

;; returns list starting with valid non-whspace-ch at head
;; second element is the number of characters skipped
(define (first-nonwhitespace-ch cl)
  (let loop ((n 0) (remcl cl))
    (if (null? remcl)
      '()
      (let ((c (car remcl)))
        (if (is-whitespace? c)
          (loop (+ n 1) (cdr remcl))
          (list remcl n))))))

;; returns (list (list parseobj remcl len) propstab)
(define (read-charlist-obj cl startn propstab)
  (let ((c (car cl)))
    (if (parse-open-paren? c)
      (parse-read-list cl startn propstab)
      (if (parse-close-paren? c)
        '() ; error list closed early
        (parse-read-symbol cl startn propstab)))))

;;; TODO: use leadsp to return the whitespace gap count

;; returns (list (list parseobj remcl len) leadsp propstab)
(define (read-charlist-next cl startn propstab)
  (let ((firstl (first-nonwhitespace-ch cl)))
    (if (null? firstl)
      '()
      (let ((obj (read-charlist-obj (car firstl) (+ startn (cadr firstl)) propstab)))
        (list (car obj) (cadr firstl) (cadr obj))))))

;; returns parseobj
;; parseobj = a single parseobj listing all objects in the file
(define (read-charlist-as-parseobj cl)
  (let loop ((allobjs '()) (remcl cl) (startn 0) (propstab '()))
    (let ((objremgot (read-charlist-next remcl startn propstab)))
      (if (null? objremgot)
        (let ((id (parseobj-freshid!)))
          (list
            (assoc-set propstab id
              (assoc-set
                (assoc-set '() 'source-start 0)
                'source-length (number->symbol (length cl))))

            (parseobj-mk (reverse allobjs) id)))

        (let ((objrem (car objremgot))
              (leadsp (cadr objremgot)))
          (loop (cons (car objrem) allobjs) (cadr objrem) (+ startn (caddr objrem) leadsp) (caddr objremgot)))))))

(define (read-charlist-from-port p)
  (let loop ((l '()))
    (let ((c (read-char p)))
      (if (eof-object? c)
        (reverse l)
        (loop (cons c l))))))

(define (read-parseobj-from-port p)
  (read-charlist-as-parseobj (read-charlist-from-port p)))

;; We should make enough of __propstab__ be a parseobj to be recognisable.
;; ie. {({__propstab__ 0} ...) 0}
;; Either that or we should just keep the propstab separate from the code.
(define (instantiate-parseobjs-from-table t l)
  (instantiate-null-parseobjs l))

(define (instantiate-null-parseobjs x)
  (if (list? x)
    (parseobj-mk (map instantiate-null-parseobjs x) parseprops-null)
    (parseobj-mk x parseprops-null)))

;; Given a list of s-expressions which may or may not contain
;; a single parseprops table at the top level,
;; Turn all other expressions (apart from the global parseprops table)
;; into parseobj's, using the parseprops table.
;; If there is no parseprops table, just insert null parseobj id for each.
(define (instantiate-parseobjs l)
  (let ((p (part (lambda (x) (and (list? x) (= (length x) 2) (eq? (car x) '__propstab__))) l)))
    (if (= (length (car p)) 1)
      (let ((t (cadaar p)))
        (list t (instantiate-parseobjs-from-table t (cadr p))))
      (list '() (instantiate-null-parseobjs l)))))

(define (read-sexprs-from-port p)
  (let loop ((rv '()))
    (let ((x (read p)))
      (if (eof-object? x)
        (instantiate-parseobjs (reverse rv))
        (loop (cons x rv))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse tree error checking
;;;
;;; These functions are used to check the parse tree for errors and warnings
;;; and then print and exit as appropriate.
;;;
;;; The parser does it's own checking for syntax errors,
;;; this phase does individual semantics checking for the passes.

;; Whilst the parser can give out some basic immediate errors,
;; this must do a complete sanity check on the parse object.
(define (parse-check-errors h p) p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parse tree strip
;;;
;;; Once we have check for semantic errors, no more semantic checking
;;; is needed if we assume that the each module in the compiler pipeline
;;; produces semantically correct code.
;;;
;;; Thus we strip parseobj's out, leaving just the parseobj-val.

(define (parse-strip-inner p)
  (let ((v (parseobj-obj p)))
    (if (p-list? v)
      (p-map parse-strip-inner v)
      v)))

(define (parse-strip-meta p)
  (parse-strip-inner
    (parseobj-mk
      (filter
        (lambda (x) (not (reserved-form-type? x '__propstab__ 2)))
        (parseobj-obj p))
      parseprops-null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler passes
;;;
;;; These perform transformations on the parse-object to perform a compiler funcion.
;;; The first and last modules of the pipeline should always use a null pass,
;;; since their function is only to read/print the code.

;; This is the null pass, used by the first and last compiler modules.
(define (parse-phase-null p) p)

; replace all occurrences of v in exp with arg,
; except those shadowed by a lambda
(define (subst exp v arg)
  (let ((x (parseobj-obj exp)))
    (if (list? x)
      (if (reserved-form-type? exp '__extern__ 2)
        exp
        (if (reserved-form-type? exp '__lambda__ 3)
          (let ((s (parseobj-obj (cadr x))))
            (if (eq? s v)
              exp
              (parseobj-sel 2 (lambda (b) (subst b v arg)) exp)))
          (parseobj-mk (map (lambda (y) (subst y v arg)) x)
            (parseobj-propsid exp))))
      (if (eq? x v)
        arg
        exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output phases
;;;
;;; The most common one will be parseobj-output-to-file since that is used
;;; to store intermediate code format.
;;; After that, there are various translating printer modules possible.

(define (parse-object-string x)
  (if (p-list? x)
    (string-append "(" (p-implode " " (p-map parse-object-string x)) ")")
    (if (number? x)
      (number->string x)
      (if (symbol? x)
        (p-symbol-string x)
        "-------UNKNOWN------"))))

(define (output-parse-object h x) (display (parse-object-string x) h))

(define (parse-output-to-port h p)
  (p-map
    (lambda (x) (begin (output-parse-object h x) (newline h)))
    p))

(define (parse-output-to-port-literally h p) (display (parse-object-string p) h))

(define (parse-output-to-port h p)
;  (map (lambda (x) (parse-output-to-port-literally h x) (newline h)) p))

  (if (null? p)
    'done
    (begin
      (write p h))))

(define (output-parseobj-list port l)
  (if (null? l)
    'done
    (begin
      (newline port)
      (write (parse-strip-inner (car l)) port)
      (output-parseobj-list port (cdr l)))))

(define (refresh-table pt px)
  pt)

;; TODO: fix up the ID's based on news parseobj syntax positions
;; then output the new table along with the raw code
(define (output-parseobjs port pt px)
  (let ((ptnew (refresh-table pt px)))
    (if (null? ptnew)
      'noop
      (write (list '__propstab__ ptnew) port))
    (output-parseobj-list port (parseobj-obj px))))

(define (display-error x)
  (display x (current-error-port)))
                
(define (die-gnose str x)
  (let ((st  (parseobj-src-start x))
         (len (parseobj-src-length x)))
    (display str (current-error-port))
    (print (parseobj-obj x) (current-error-port))
    (display (if (eq? st #f) "" (string-append " at char:" (symbol->string st))) (current-error-port))
    (newline (current-error-port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
