structure Ast = struct

type id = string

structure IdKey = struct 
    type ord_key = id
    val compare = String.compare
end

datatype BinOp = ADD | SUB | MUL | DIV | LSHIFT 
    | RSHIFT | EQ | NE | G | L | GE | LE | AND | OR

datatype Lvalue = Var of id
    | MemberRef of Lvalue * id
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
    | For of id * Exp * Exp * Exp
    | Break

    | Lval of Lvalue

    | FunCall of id * Exp list
    | MethodCall of Lvalue * id * Exp list

    | LetStmt of Dec list * Exp list
    | New of id

    | Assignment of Lvalue * Exp

and Dec = Vardec of id * id option * Exp
    | Typedec of id * Type
    | Import of id
    | FuncDec of id * (id * id) list * id option * Exp
    | ClassDef of id * id option * ClassField list
    | PrimitiveDec of id * (id * id) list * id option

and ClassField = MethodDec of id * (id * id) list * id option * Exp
    | VarDecCF of id * id option * Exp

and Type = Alias of id
    | Array of id
    | RecordType of (id * id) list
    | ClassType of id option * ClassField list

datatype Prog = Expression of Exp | Decs of Dec list

end
