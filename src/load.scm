
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

;;; merges file "foo"'s contents inito this one
;;; this phase could also be called "include"
;;; (load "foo")

;;; This method of loading code is here for backwards
;;; compatibility with Scheme.
;;; Use the module system once it becomes implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remap all propsids in x using rmap
(define (parseobj-remap px rmap)
  (let ((x (parseobj-obj px)))
    (parseobj-mk
      (if (list? x)
        (map (lambda (y) (parseobj-remap y rmap)) x)
        x)
      (assoc-ref rmap (parseobj-propsid px) 0))))

;; returns (list px pt)
(define (load-with-table-file py pt fnmsym)

  ;; what we want to do is merge all of these objects' properties
  ;; into the current file's properties table.
  (let* ((pt2 (parseobj-propstable py))
          (rmap (propstable-remap pt pt2))
          (ptrv (propstable-add pt pt2 rmap)))

    (let ((rvs (foldr
                 (lambda (x acc)
                   (let ((rv (perform-loads (parseobj-remap x rmap) (cadr acc))))
                     (list (cons (car rv) (car acc)) (cadr rv))))
                 (list (list) ptrv)
                 (filter (lambda (x) (not (reserved-form-type? x '__propstab__ 2))) (parseobj-obj py)))))
           (list (parseobj-mk (car rvs) (parseobj-propsid py)) (cadr rvs)))))

;; a load with table which does not need to merge props into pt
(define (load-with-table py pt)
  (let ((rvs (foldr
               (lambda (x acc)
                 (let ((rv (perform-loads x (cadr acc))))
                   (list (cons (car rv) (car acc)) (cadr rv))))
               (list (list) pt)
               (parseobj-obj py))))
    (list (parseobj-mk (car rvs) (parseobj-propsid py)) (cadr rvs))))
 
;; returns (list px pt)
(define (perform-loads px pt)
  (let ((x (parseobj-obj px)))
    (if (and (list? x) (>= (length x) 2))
      (let ((carx (parseobj-obj (car x))))
        (if (eq? carx '__load__)
          (let* ((fnmsym (parseobj-obj (cadr x)))
                  (filepxl (load-with-table-file (read-sexprs-from-port (open-input-file (symbol->string fnmsym))) pt fnmsym))
                  (filepx  (car filepxl))
                  (filelpx (parseobj-obj filepx)))

            (list
              (parseobj-mk
                (cons (parseobj-mk '__begin__ parseprops-null) filelpx)
                (parseobj-propsid px))

              (cadr filepxl)))

          (if (eq? carx '__begin__)
            ;; this does a (perform-loads '__base__) but thats harmless
            (load-with-table px pt)
            (list px pt))))
      (list px pt))))

(define (parse-phase-load py)
  (let* ((pt (parseobj-propstable py))
          (rv (load-with-table py pt)))
    (parseobj-conv
      (lambda (xs)
        (map
          (lambda (px)
            (if (reserved-form-type? px '__propstab__ 2)
              (parseobj-sel 1 (lambda (x) (cadr rv)) px)
              px))
          xs))
      (car rv))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main

(define (start)
  (parse-output-to-port (current-output-port)
    (parse-phase-load
      (read-sexprs-from-port (current-input-port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
