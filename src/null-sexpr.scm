
(load "base.scm")

(define (start)
  (let ((pxl (read-sexprs-from-port (current-input-port))))
    (output-parseobjs (current-output-port)
      (car pxl)
      (cadr pxl))))
