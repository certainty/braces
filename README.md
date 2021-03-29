# Braces

[![Build Status](https://travis-ci.com/certainty/braces.svg?token=4u7ZeAMZynyk9xmAA6Tm&branch=main)](https://travis-ci.com/certainty/braces)


> Brace for impact


## TODO

- [ ] Compile lambdas
    * this will affect the way we represent compilations and should be done first
- [ ] Principled VM design 
    * account for interactive mode in the repl (also setting values which are visible across invocations)
    * account for the printer needing access to the symbol table 
- [ ] Fix environment implementation. Set should also traverse scopes 
- [ ] Clean up function calls 
    * make sure argument counts are checked and functions don't access data on the stack that they shouldn't
- [ ] Bring the error reporting and error handling story up a notch 
    * (find appropriate representations (hierarchy) in the code and surface useful messages)
    * devise a clear model on when / how things fail in the compiler and the vm
- [ ] Make constant table more efficient
    * currently constants are stored in a vector in the chunk without being deduplicted. This might be fine since the number of constants in a chunk is potentially small.
      However I think a table for all the constants could be a better, more uniform, approach. 

