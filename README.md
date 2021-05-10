# Braces

![Build Status](https://https://github.com/certainty/braces/blob/switch-to-actions/.github/workflows/rust.yml/badge.svg?branch=main)

> Brace for impact

## TODO

- [ ] literals
  - [x] bool
  - [x] symbol
  - [x] char
  - [ ] number
  - [ ] vector
  - [ ] byte-vector
  - [x] proper list
  - [ ] improper list
  - [x] string
  - [x] quoted constants
- [x] variables
- [x] conditionals
- [x] lambda expression
- [x] procedure call
- [x] assignment

### Closures
- [ ] close up values `(define test ((let x #t)) (let ((closure (lambda () (set! x (not x))))) (set! x (not x)) closure))`
- [ ] make mutual reference of two procedures work with closures
- [ ] make sure we don't create a memory leak


## Bugs

- [x] set! unbound variables should create an error
