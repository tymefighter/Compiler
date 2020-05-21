structure Ast = struct

type id = string

datatype BinOp = ADD | SUB | MUL | DIV | EQ | NE | G | L | GE | LE | AND | OR

datatype Lvalue = Var of string
    | MemberRef of Lvalue * string
    | IdxArr of Lvalue * Exp

and Exp = LiteralNil
    | LiteralInt of int
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
    | New of id

    | Assignment of Lvalue * Exp

and Dec = Vardec of id * id option * Exp
    | Typedec of id * Type
    | Import of id
    | FuncDec of id * (string * string) list * id option * Exp
    | ClassDef of string * string option * ClassField list
    | PrimitiveDec of string * (string * string) list * id option

and ClassField = MethodDec of string * (string * string) list * id option * Exp
    | VarDecCF of id * id option * Exp

and Type = Alias of string
    | Array of string
    | RecordType of (string * string) list
    | ClassType of id option * ClassField list

datatype Prog = Expression of Exp | Decs of Dec list

end
