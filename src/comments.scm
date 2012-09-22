
; this will be removed before converting to smooth
(load "schemeup.scm")

(load "lambda.scm")
(load "bool.scm")
(load "pair.scm")
(load "numeral.scm")
(load "list.scm")
(load "basicio.scm")

(define (n59 f) (lambda (x) (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define semicolon n59)
(define newlinech n10)

(define (chareq? a) (lambda (b) ((= a) b)))

; for the very time being we can get away with using 0 instead of -1 for EOF
(define (iseof? x) (n0? x))

(define comment-char semicolon) ; In the distant future this will be changed to #
; TODO: (define comment-char hash)

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

(define read-comment
  (Y
    (lambda (read-comment)
      ((>>= getc)
        (lambda (c)
          (((fif (iseof? c))
             (return id))
            (((fif ((chareq? c) newlinech))
               ((>> (putc c)) read-normal))
              read-comment)))))))

(define read-normal
  (Y
    (lambda (read-normal)
      ((>>= getc)
        (lambda (c)
          (((fif (iseof? c))
             (return id))
            (((fif ((chareq? c) comment-char))
                 read-comment)
              ((>> (putc c)) read-normal))))))))

(define (start) (read-normal n0))
