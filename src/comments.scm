
;; In the distant future this will be changed to #
(define comment-char #\;)

; we are in a comment
(define (read-comment in out)
  (let ((c (read-char in)))
    (if (eof-object? c)
      #f ;done
      (if (eq? c #\newline)
        (begin
          (write-char c out)
          (read-normal in out))
        (read-comment in out)))))

; we are not in a comment
(define (read-normal in out)
  (let ((c (read-char in)))
    (if (eof-object? c)
      #f ;done
      (if (eq? c comment-char)
        (read-comment in out)
        (begin
          (write-char c out)
          (read-normal in out))))))

(define (start)
  (let ((in (current-input-port))
         (out (current-output-port)))
    (read-normal in out)))
