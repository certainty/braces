# Braces (the tiny scheme compiler and VM)



> Brace for impact


## State 
This is beyond the state where it's just dabbling around. However at this point I don't have any intentions
to turn it into something for real world usage (I'm not sure I could even if I tried). 
It will likely remain a play ground where I just try out ideas. I'll just see where it takes me.


## Usage

Start the REPL and play around with it

```
make repl 
```

### Language Support 

- [x] literals
  - [x] bool
  - [x] symbol
  - [x] char
  - [x] number
  - [x] vector
  - [x] byte-vector
  - [x] proper list
  - [x] improper list
  - [x] string
  - [x] quoted constants
- [x] variables
- [x] conditionals
- [x] lambda expressions
- [x] procedure calls
- [x] assignments
- [x] quasi-quoted expression

### Compiler 
- [x] datum (s-expression) parser
- [x] expression parser 
- [x] basic code generator 
- [ ] collect all errors in reader  
- [ ] collect all errors in parser 
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
- [ ] improve error reporting for reader
- [ ] improve error reporting for expressions

### VM

- [x] optimise tail calls
- [x] closures 
- [ ] continuations 
- [ ] optimize stack 

### Debugger 
- [ ] generate DWARF debug information 

### GC
Well there is none :D
I probably should build one or maybe not.


### Repl
- [x] command support
- [x] bracket matching
- [x] completion of bound identifiers

### Continuations
- [ ] figure out how to implement them

### Problems
There are plenty :D 

* Equality isn't correctly implemented -> I will add a test file that verifies that the language implements as the r7rs specifies it, which will cover equality as well.
* Runtime representation of values isn't at all optimised. It likely uses way too many allocations and holds the types wrong
* The parser bails after the first error (you'd want to collect all)
* I'm not sure if the custom ParseResult I use is really needed? (I will have to figure that out)
* No proper macro expander yet. (There is an expansion phase which expands using rust code but that's very limited for now)
* No support for internal defines (we might get away by adding an expansion to letrec*)
* I'm not sure the location tracking works all the time


### Questions
* let/let*/letrec currently expand to the corresponding lambda expressions. Should it not do that and instead I handle those as special cases in the VM. Would that yield benefits other than being easier to trace and potentially better error reporting?
