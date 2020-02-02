structure Ast = struct

type id = string

datatype BinOp = ADD | SUB | MUL | DIV | EQ | NE | G | L | GE | LE | AND | OR

datatype Lvalue = Var of string
    | MemberRef of Lvalue * string
    | IdxArr of Lvalue * Exp

and Exp = LiteralNil
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

    | Lval of Lvalue

    | FunCall of string * Exp list
    | MethodCall of Lvalue * string * Exp list

and Dec = Vardec of id * id option * Exp

datatype Prog = Expression of Exp | Decs of Dec list

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