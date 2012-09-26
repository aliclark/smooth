
(define iocons      (extern io_iocons))
(define iocar       (extern io_iocar))
(define iocdr       (extern io_iocdr))
(define stdin       (extern io_stdin))
(define stdout      (extern io_stdout))
(define fgetb       (extern io_fgetb))
(define fputb       (extern io_fputb))
(define inttochurch (extern io_inttochurch))
(define churchtoint (extern io_churchtoint))

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

(define getb (fgetb stdin))
(define putb (fputb stdout))

(define getc
  ((>>= getb)
    (lambda (c)
      (return (inttochurch c)))))

(define (putc c)
  (putb (churchtoint c)))
