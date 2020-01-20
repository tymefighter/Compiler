structure Ast = struct

    datatype Literal = Integer of int
        | Str of string
        | Nil

    datatype Op = ADD | SUB | MUL | DIV | EQ | NE | G | L | GE | LE | AND | OR

    type Id = string

    datatype Lvalue = Var of Id
        | Member of Lvalue * Id

    datatype Exp = Lit of Literal
        | IfThen of Exp * Exp
        | IfThenElse of Exp * Exp * Exp
        | While of Exp * Exp
        | For of Id * Exp * Exp * Exp
        | Break
        | Assign of Lvalue * Exp
        | FunctionCall of Id * Exp list
        | MethodCall of Lvalue * Id * (Exp list)
        | VarExp of Lvalue
        | OpApp of Exp * Op * Exp

end