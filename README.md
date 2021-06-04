# Braces

![Build Status](https://https://github.com/certainty/braces/blob/switch-to-actions/.github/workflows/rust.yml/badge.svg?branch=main)

> Brace for impact
> 

## Usage

Start the REPL and play around with it

```
make repl 
```

## TODO

- [ ] literals
  - [x] bool
  - [x] symbol
  - [x] char
  - [x] number
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

### Repl

- [x] command support
- [x] bracket matching
- [x] completion of bound identifiers

### TCO
- [x] identify tail calls
- [x] optimise tail calls

### Errors
- [ ] fix backtraces 
- [ ] improve error reporting for sexp parser
- [ ] improve error reporting for expressions

### Macros
- [ ] add macro expansion phase
- [ ] add syntactic closure macro system

### Continuations
- [ ] figure out how to implement them

### Language features
- [ ] fully fledged define for procedures
- [ ] more list procedures


### Closures
- [x] close up values `(define test ((let x #t)) (let ((closure (lambda () (set! x (not x))))) (set! x (not x)) closure))`
- [x] make mutual reference of two procedures work with closures
- [x] make sure we don't create a memory leak


## Bugs

- [x] set! unbound variables should create an error
