structure Ast = struct

type id = string

datatype BinOp = ADD | SUB | MUL | DIV | EQ | NE | G | L | GE | LE | AND | OR

datatype Exp = LiteralNil
    | LiteralInt of string
    | LiteralStr of string

    | Op of Exp * BinOp * Exp
    | NegExp of Exp
    | Exprs of Exp list

    | IfThen of Exp * Exp
    | IfThenElse of Exp * Exp * Exp
    | While of Exp * Exp
    | For of string * Exp * Exp * Exp
    | Break

datatype Prog = Expression of Exp

(* datatype Exp = LiteralNil
    | LiteralInt of string
    | LiteralStr of string
    
    | IfThen of Exp * Exp 
    | IfThenElse of Exp * Exp * Exp
    | While of Exp * Exp
    | For of id * Exp * Exp * Exp
    | Break

    | NegExp of Exp
    | Op of Exp * BinOp * Exp
    | RoundBracket of Exp *)


end