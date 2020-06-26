# Syntax of the SubTiger© Language


# Grammer of the SubTiger© Language

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
