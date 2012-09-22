
; Why this isn't valid Smooth code:
; * many reasons.
; * implement an io monad before proceeding.

(define (zero f x) x)
(define (fifty-nine f x) (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define false (lambda (x) (lambda (y) y)))
(define true  (lambda (x) (lambda (y) x)))
(define semicolon #\;)
(define comment-char semicolon) ; In the distant future this will be changed to #
(define newlinech #\newline)
(define stdin (current-input-port))
(define stdout (current-output-port))

(define (getc z) (read-char stdin))
(define (putc c z) (write-char c stdout))

(define (chareq? a b) (eq? a b))

; need to upgrade to negativeable church numerals first
(define (iseof? x) (eof-object? x))

(define (read-comment z)
  (let ((c (getc z)))
    (if (iseof? c)
      false ;done
      (if (chareq? c newlinech)
        (begin
          (putc c z)
          (read-normal z))
        (read-comment z)))))

(define (read-normal z)
  (let ((c (getc z)))
    (if (iseof? c)
      false ;done
      (if (chareq? c comment-char)
        (read-comment z)
        (begin
          (putc c z)
          (read-normal z))))))

(define (start) (read-normal zero))
