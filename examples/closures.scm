(define set-x #t)
(define get-x #t)


(let ((x 0))
  (set! set-x (lambda (n) (set! x n)))
  (set! get-x (lambda () x)))

