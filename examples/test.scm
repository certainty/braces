; this is a comment
(define identity (lambda (x) x))

(identity "foo")

(define do-it (lambda (x) (if x 'heads 'tails)))

(do-it #t)
(do-it #f)

