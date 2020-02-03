structure Ast = struct

type id = string

datatype BinOp = ADD | SUB | MUL | DIV | EQ | NE | G | L | GE | LE | AND | OR

datatype Type = Alias of string
    | Array of string
    | RecordType of (string * string) list

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

    | LetStmt of Dec list * Exp list

and Dec = Vardec of id * id option * Exp
    | Typedec of id * Type
    | Import of id
    | FuncDec of id * (string * string) list * id option * Exp
    | ClassDef of string * string option * ClassField list

and ClassField = MethodDec of string * (string * string) list * id option * Exp

datatype Prog = Expression of Exp | Decs of Dec list

end