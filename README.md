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

## TODO

### Compiler 
- [x] datum (s-expression) parser
- [x] expression parser 
- [x] basic code generator 
- [x] identify tail calls
- [x] quasi quote
- [ ] make quasi quoting bullet proof (it might have edge cases at the moment)
- [ ] collect all errors in reader
- [ ] collect all errors in parser 
- [ ] bytecode serialization (ahead of time compilation)
- [ ] library support 
- [ ] include / include-ci
- [ ] cond-expand 
- [ ] prepare code for evolution (next rNrs and extensions)
- [ ] optimization pass 
- [ ] CPS conversion

#### Macros
- [x] add macro expansion phase
- [x] add procedural macro system (explicit renaming?)

#### Errors
- [ ] fix backtraces 
- [ ] improve error reporting for reader
- [ ] improve error reporting for expressions

### VM

- [x] optimise tail calls
- [x] closures 
- [ ] continuations 

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

## Problems
There are plenty :D 

* Runtime representation of values isn't at all optimised. It likely uses way too many allocations and holds the types wrong
* The parser bails after the first error (you'd want to collect all)
* I'm not sure if the custom ParseResult I use is really needed? (I will have to figure that out)
* No support for internal defines (we might get away by adding an expansion to letrec*)
* I'm not sure the location tracking works all the time


## Questions
* let/let*/letrec currently expand to the corresponding lambda expressions. Should it not do that and instead I handle those as special cases in the VM. Would that yield benefits other than being easier to trace and potentially better error reporting?

## Optimisations
Once I have the language in a place where it's correct, I can look at more optimizations. I'm still learning rust and some things I simply didn't know when I 
started.

- [ ] make use of COW or even SUPERCOW for values 
- [ ] make sure that references are used correctly to minimise cloning
