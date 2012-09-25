
(define (extern x) x)

(define nativecons cons)
(define nativecar  car)
(define nativecdr  cdr)
(define native=    =)
(define native-    -)
(define native+    +)
(define native>    >)

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

(define io_stdin  (current-input-port))
(define io_stdout (current-output-port))

(define io_fgetb_tabl (make-table))
(define io_fgetb_z 0)
(define (io_fgetb f)
  (lambda (z)
    (let ((nexz (native+ z 1)))
      (if (native> io_fgetb_z z)

        ((io_iocons (table-ref io_fgetb_tabl z)) nexz)

        (let* ((c (read-char f))
                (ch (if (eof-object? c) 0 (char->integer c))))

          (set! io_fgetb_z nexz)
          (table-set! io_fgetb_tabl z ch)

          ; for the very time being we can get away
          ; with using 0 instead of -1 for EOF
          ((io_iocons ch) nexz))))))

(define io_fputb_z 0)
(define (io_fputb f)
  (lambda (c)
    (lambda (z)
      (let ((nexz (native+ z 1)))
        (if (native> io_fputb_z z)

          ((io_iocons #f) nexz)

          (begin
            (set! io_fputb_z nexz)
            (write-char (integer->char c) f)
            ((io_iocons #f) nexz)))))))
