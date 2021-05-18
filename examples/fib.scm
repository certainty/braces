(define + (lambda (a b) (fx+ a b)))
(define - (lambda (a b) (fx- a b)))
(define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)
