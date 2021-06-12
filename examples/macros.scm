(define-syntax define-procedure 
  (er-macro-transformer
   (lambda (exp rename compare) #t)))


(define-procedure (foo (x y) y))

