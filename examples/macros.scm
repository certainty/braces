(define-syntax my-begin
  (sc-macro-transformer
   (lambda (exp env)
     `((lambda () ,@exp)))))


(define (foo x y z) (+ x y))
