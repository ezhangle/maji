### DESCRIPTION (Work In Progress)

**maji** is a simple programming language loosely based on C, but without header-files (order dependency).
There is also a basic type-inference system.

It currently outputs standalone bytecode executable files that can be run by **majivm**.

The compiler currently takes one or more files as input argument, along with an optional output filename.

Build maji:
```
# clone repo and build binary
git clone https://github.com/koekeishiya/yabai
make install      # release version
make              # debug version
```

To compile a *.maji* source file:
```
    ./bin/majic <input> -o <a.bcr>
```

To run a generated bytecode executable:
```
    ./bin/majivm -p a.bcr
```

### DISCLAIMER

This is primarily a project for learning purposes (and for fun), and is in no way intended to replace a real programming language.
