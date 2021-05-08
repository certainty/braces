(define get-x '())
(define set-x '())

(let ((x #t))
  (set! get-x (lambda () x))
  (set! set-x (lambda (v)
                (set! x v))))
(set-x #\a)
(get-x)

