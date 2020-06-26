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
You may run the compiler on an input tiger source as follows:

```
    ./tiger < program.subtig > program.mips
```

## Raising Issues

You may raise issues here: https://github.com/tymefighter/Compiler/issues

## Design

Given in `Compiler/README.md`

## Author

tymefighter - Ahmed Zaheer Dadarkar, IIT Palakkad