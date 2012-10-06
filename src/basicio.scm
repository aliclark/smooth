
(define inttochar   (extern io_inttochar))
(define chartoint   (extern io_chartoint))
(define inttochurch (extern io_inttochurch))
(define churchtoint (extern io_churchtoint))
(define iocons      (extern io_iocons))
(define iocar       (extern io_iocar))
(define iocdr       (extern io_iocdr))
(define stdin       (extern io_stdin))
(define stdout      (extern io_stdout))
(define fgetch      (extern io_fgetch))
(define fputch      (extern io_fputch))

(define (return x) (lambda (r) ((iocons x) r)))

(define (>>= m)
  (lambda (f)
    (lambda (r)
      (let ((v1 (m r)))
        ((f (iocar v1)) (iocdr v1))))))

(define (make->> f)
  (lambda (m1)
    (lambda (m2)
      ((f m1) (lambda (x) m2)))))

(define >> (make->> >>=))

(define getch (fgetch stdin))
(define putch (fputch stdout))

(define getc
  ((>>= getch)
    (lambda (c)
      (return (inttochurch (chartoint c))))))

(define (putc c)
  (putch (inttochar (churchtoint c))))
