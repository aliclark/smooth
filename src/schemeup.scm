
(define (extern x) x)

(define nativecons cons)
(define nativecar  car)
(define nativecdr  cdr)
(define native=    =)
(define native-    -)
(define native+    +)

;; IO lib

(define (io_iocons v) (lambda (z) (nativecons v z)))

(define (io_iocar x) (nativecar x))
(define (io_iocdr x) (nativecdr x))

(define (io_inttochurch i)
  (lambda (f)
    (lambda (x)
      (let loop ((k i) (c x))
        (if (native= k 0) c (loop (native- k 1) (f c)))))))

(define (io_churchtoint c) ((c (lambda (x) (native+ x 1))) 0))

(define io_stdin (current-input-port))
(define io_stdout (current-output-port))

(define io_fgetb (lambda (f) (lambda (z)
                               (let* ((c (read-char f))
                                       (n (if (eof-object? c) 0 (char->integer c))))
                                 ((io_iocons n) z)))))
(define io_fputb
  (lambda (f)
    (lambda (c)
      (lambda (z)
        ((io_iocons
           (write-char (integer->char c) f))
          z)))))
