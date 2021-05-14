(define fails (lambda () (not)))

(define foo (lambda () (fails)))

(define bar (lambda ()
              (let ((x #t))
                (foo))))


(bar)
