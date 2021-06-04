(define fib-tc (lambda (n)
  (fib-iter 1 0 n)))

(define fib-iter (lambda (a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1)))))


(define fib (lambda (n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)
