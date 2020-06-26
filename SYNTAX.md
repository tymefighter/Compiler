# Syntax of the SubTiger Language

Everything in SubTiger is actually an expression, and hence always evaluates
to some value. Now, we may look some sample code to understand the syntax
better:

1. We can use a variable only after declaration, and we can place declarations
with the let block of a let statement. Within the in-end block of the let
statment, we place an expression. Wherever we can place an expression, we can
place a tuple of expressions seperated by semicolons which would be evaluated
for "side-effects" but only the last expression within the tuple's value would
be returned as a part of the expression. The `print` function is a build-in
function that prints the integer expression's value after evaluating.

```
    let
        var a := 1
        var b := 0
        var c := 0
    in
        (
            b := a * 2;
            c := b * 3;
            print (b + c)
        )
    end
```

2. While-loops are also expressions, hence can be a part of in-end part of a
let block. The println function is a built-in function which prints expressions
and appends a new line.

```
    let
        var i := 0
    in
        while (i < 5) do
        (
            println(i);
            i := i + 1
        )
    end
```

3. For-loops are also expressions, hence can be a part of in-end part of a
let block. The println function is a built-in function which prints expressions
and appends a new line. The for loop below starts at 0 and ends at n - 1 (both
inclusive)

```
    let
        var n := 10
    in
        for i := 0 to n do
            println (i)
    end
```

4. Function declarations are part of the declaration statements, hence have
to be in the let-in block of the let statement, for example

```
    let
        function add (x : int, y : int) = x + y
        function sub (x : int, y : int) = x - y
    in
        println (add (1, 2))
    end
```

5. We can have nested function declarations as follows
```
    let
        var u := 1
        function A (x : int) = 
            let
                function B (y : int) = 
                    let
                        function C (z : int) = z + u
                    in
                        C (y) + u
                    end
            in
                B (x) + u
            end
    in
        A (5)
    end
```

# Example Programs

Please refer to the example programs in the directory `Compiler/Test`

# Grammer of the SubTiger Language

- [,] stands for zero or once, eg: ['1'] could be '', or '1'
- {,} stands for zero or more, eg: {'1'} could be '', '1', '11', '111', ...

```
program ::=  
    exp  
  | decs  

exp ::=  
    integer  
  | lvalue  
  | id ( [ exp { , exp }] )  
  | - exp  
  | exp op exp  
  | ( exps )  
  | lvalue := exp  
  | if exp then exp [else exp]  
  | while exp do exp  
  | for id := exp to exp do exp  
  | break  
  | let decs in exps end  

lvalue ::= id

exps ::= [ exp { ; exp } ]

decs ::= { dec }
dec ::=  
    vardec  
  | function id ( tyfields ) [ : type-id ] = exp

vardec ::= var id [ : type-id ] := exp

tyfields ::= [ id : type-id { , id : type-id } ]

type-id ::= id

op ::= + | - | * | / | % | << | >> | = | <> | > | < | >= | <= | & | |
```
