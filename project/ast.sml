structure Ast = struct

    datatype Binop = ADD | SUB | MUL | DIV
    datatype ConOp = G | GE | L | LE | E | NE | AND | OR

    datatype Expr = Const of int
        | ApplyOp of Expr * Binop * Expr

    datatype ConditionExpr = Expr
        | ApplyConOp of ConditionExpr * ConOp * ConditionExpr
    
    datatype Asgn = Assign of string * Expr

    datatype Control = IfThen of Condition * Stmt
        | IfThenElse of Condition * Stmt * Stmt
        | While of Condition * Stmt
        | For of 