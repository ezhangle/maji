### DESCRIPTION (Work In Progress)

**Maji** is a simple programming language loosely based on C, but without header-files (order dependency).
There is also a basic type-inference system.

It currently outputs bytecode executable files that can be run by [bytecode runner](https://github.com/koekeishiya/bytecode).
The source for the *bytecode runner* is also included in the *bytecode* subfolder,
but there is currently no makefile instruction for it in this repository.

The compiler currently takes one or more files as input argument, along with an optional output filename.

To compile a *.maji* source file:

```
   ./majic <input> -o <a.out>
```

### DISCLAIMER

This is primarily a project for learning purposes (and for fun), and is in no way intended to replace a real programming language.
