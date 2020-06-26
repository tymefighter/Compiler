# Installing the SubTiger© compiler provided in this repository

The source of the compiler is written in Standard ML, hence
if you want to build the compiler, you need to have `mlton`
installed on your system. If you do not have `mlton` installed
then please installing by following the steps mentioned in the
following link: 

```
    http://mlton.org/Installation
```

Now, once you have mlton installed in your system, proceed as
follows: -

1. cd Compiler
2. make tiger

Now, the compiler executable would be generated in the Compiler
directory: -

```
    Compiler/tiger
```

Now, you may compile a test program as follows (please remain
in the Compiler directory): -

```
    ./tiger < Test/test_case0 > testOut.s
```

Now, you may run the generated mips assembly code `testOut.s` in your
mips assembly running tool (for example `QtSpim`).
