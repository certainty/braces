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
This is beyond the toy state but the code is messy and experimental at times.
However, the compiler and the VM is coming together and I will go on with this to see 
how fare I can bring it. 

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
- [ ] libraries  
- [ ] optimization pass 
- [x] identify tail calls
- [x] quasi quote 

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

### Problems
There are plenty :D 

* Runtime representation of values isn't at all optimised. It likely uses way too many allocations and holds the types wrong
* The parser bails after the first error (you'd want to collect all)
* I'm not sure if the custom ParseResult I use is really needed? (I will have to figure that out)
* No proper macro expander yet. (There is an expansion phase which expands using rust code but that's very limited for now)
* No support for internal defines (we might get away by adding an expansion to letrec*)
* I'm not sure the location tracking works all the time
