# Compiling to Reverse Polish notation (RPN)


This directory is an illustrative example of a small compiler: a
compiler from expressions to reverse polish notation. Although this is
really a toy example, we have structured the program like a classical
compiler. More complicated compilers can follow this high level
structure.

## The main programs.

This directory contains the source code for two standalone programs.

`ec`
:   The expression compiler which compiles normal expressions into
    reverse polish notation. You should think of the expression as
    your high level language and reverse polish notation as your low
    level language.

`rp`
:   The reverse polish machine, which interprets the expression in
    reverse polish notation. This is given to test your compiler.

Build these programs using the following make command

```
make all
```

## The RPN machine.

The RPN takes a sequence of machine instructions which consists of

1. A number with an optional sign [+-~]. One can use the ~ sign for
   negation following the Standard ML convention

2. Single character commands `p` (for printing the top of the stack)
   `s` for printing the entire stack and `c` for clearing the stack.

3. Line comments starting with the character `#`.

The executable `rp` that this repository provides can be used to run a
_reverse polish script_. For example, here is a "machine language
program" for `rp` that illustrates its syntax.

## Testing

To test if the compiler as well as the rpn machine is working correctly,
we may add test cases to the `test_cases` file and the corresponding 
value of the expression to `test_actual` file. Then we may test the
system by running the following commands on the terminal: -

```
    ./ec < test_cases | ./rp > test_output
    diff test_output test_actual
```

If there is a difference, then there is an error in the code

# Reference

[ast]: <ast.sml>
[machine]: <machine.sml>
[translate]: <machine.sml>
[expr.grm]: <expr.grm>
[expr.lex]: <expr.lex>
[mlyacc]: <http://mlton.org/MLYacc>
[mllex]: <http://mlton.org/MLLex>