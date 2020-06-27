# Compiler for SubTiger Language

The SubTiger language is the subset of the Tiger language
(https://www.lrde.epita.fr/~tiger/tiger.html), it supports
features like nested functions, while loops, for loops,
control statements like if-then, if-then-else. It is adds
a functional programming flavour to the regular imperative
language functionality. Currenlty, the language supports
only integers, but would be extended to strings and floating
point computation. Currenlty, the compiler compiles to
`mips assembly` code.

## Technologies Used

* Standard ML 

## Installation 

Given in `INSTALL.md`

## Syntax for the Language

Given in `SYNTAX.md`

## Using the Compiler

Once the installation is complete, the `tiger` executable
in the `Compiler` directory contains the compiler executable.
You may run the compiler on an input tiger source as follows
to get the output MIPS assembly code in `program.s`

```
    ./tiger program.tig
```

You can use the compiler flags to generate the file with your
own output file name, also you may generate the intermediate
code

- `-o prog.s` would output the generated MIPS code in `prog.s`
- `-i prog.i` would output the generated Tree Intermediate
code in `prog.i`.

Examples

This would place the intermediate code in `inter.i` and
MIPS code in `mipsCode.s`.

```
    ./tiger program.tig -i inter.i -o mipsCode.s
```

Order of the flags don't matter.

```
    ./tiger program.tig -o mipsCode.s -i inter.i
```

By default, the output code is outputted in the file
with same prefix as input code if the output file is
not specified. Hence, the output MIPS code would
be in `program.s`

```
    ./tiger program.tig -i inter.i
```

By default, the intermediate code is **not** generated

```
    ./tiger program.tig -o mipsCode.s
```

## Raising Issues

You may raise issues here: https://github.com/tymefighter/Compiler/issues

## Design

Given in `Compiler/README.md`

## Author

tymefighter - Ahmed Zaheer Dadarkar, IIT Palakkad