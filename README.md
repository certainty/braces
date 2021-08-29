# Braces

![Build Status](https://https://github.com/certainty/braces/blob/switch-to-actions/.github/workflows/rust.yml/badge.svg?branch=main)

> Brace for impact
> 

## Usage

Start the REPL and play around with it

```
make repl 
```

## State 

### Language Support 

- [ ] literals
  - [x] bool
  - [x] symbol
  - [x] char
  - [x] number
  - [x] vector
  - [ ] byte-vector
  - [x] proper list
  - [x] improper list
  - [x] string
  - [x] quoted constants
- [x] variables
- [x] conditionals
- [x] lambda expression
- [x] procedure call
- [x] assignment

### Compiler 
- [x] sexp parser
- [x] expression parser 
- [x] basic code generator 
- [ ] collect all errors in sexp parser 
- [ ] collect all errors in expr parser 
- [ ] bytecode serialization
- [ ] Module System 
- [ ] optimization pass 
- [x] identify tail calls
- [ ] quasiquote / Quote 

#### Macros
- [ ] add macro expansion phase
- [ ] add syntactic closure macro system

#### Errors
- [ ] fix backtraces 
- [ ] improve error reporting for sexp parser
- [ ] improve error reporting for expressions

### VM

- [x] optimise tail calls
- [x] closures 
- [ ] continuations 
- [ ] optimize stack 

### Debugger 
- [ ] generate DWARF debug information 


### Repl
- [x] command support
- [x] bracket matching
- [x] completion of bound identifiers

### Continuations
- [ ] figure out how to implement them
