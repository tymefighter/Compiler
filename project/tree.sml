structure Tree = struct

    datatype exp = CONST of int
        | NAME of Temp.label
        | TEMP of Temp.temp
        | BINOP of binop * exp * exp
        | MEM of exp
        | CALL of exp * exp list
        | ESEQ of stm * exp
    
    and stm = MOVE of exp * exp
        | EXP of exp
        | JUMP of exp * Temp.label list
        | CJUMP of relop * exp * exp * Temp.label * Temp.label
        | SEQ of stm * stm
        | LABEL of Temp.label

    and binop = PLUS | MINUS | MUL | DIV
        | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
    
    and relop = EQ | NE | LT | GT | LE | GE
        | ULT | ULE | UGT | UGE
    
end

structure Translate = struct

    exception EmptySeq

    datatype exp = Ex of Tree.exp
        | Nx of Tree.stm
        | Cx of Temp.label * Temp.label -> Tree.stm

    fun seq [st] = st
        | seq (st :: st_list) = Tree.SEQ (st, seq st_list)
        | seq [] = raise EmptySeq

    fun unEx (Ex e) = e
        | unEx (Nx s) = Tree.ESEQ (s, Tree.CONST 0)
        | unEx (Cx con) = let
                
                val res = Temp.newtemp () (* Result of conditional exp *)
                val t = Temp.newlabel () (* True then jump here *)
                val f = Temp.newlabel () (* False then jump here *)

            in
                Tree.ESEQ (seq [
                    Tree.MOVE (Tree.TEMP res, Tree.CONST 1),
                    con (t, f),
                    T.LABEL f,
                    Tree.MOVE (Tree.TEMP res, Tree.CONST 0),
                    T.LABEL t
                ], Tree.TEMP res)
            end
   
     fun translate (Ast.LiteralInt x) = Ex (Tree.CONST x)
        | translate (Ast.Op e1 bin_op e2) = let
                val ex1 = unEx (translate e1)
                val ex2 = unEx (translate e2)
            in 
                case bin_op of
                    ADD => Tree.BINOP (Tree.PLUS, ex1, ex2)
                    | SUB => Tree.BINOP (Tree.MINUS, ex1, ex2)
                    | MUL => Tree.BINOP (Tree.MUL, ex1, ex2)
                    | DIV => Tree.BINOP (Tree.DIV, ex1, ex2)
                    | EQ => unEx (Cx (fn (t, f) => CJUMP (Tree.EQ, ex1, ex2, t, f)))
                    | NE => unEx (Cx (fn (t, f) => CJUMP (Tree.NE, ex1, ex2, t, f)))
                    | G => unEx (Cx (fn (t, f) => CJUMP (Tree.GT, ex1, ex2, t, f)))
                    | L => unEx (Cx (fn (t, f) => CJUMP (Tree.LT, ex1, ex2, t, f)))
                    | GE => unEx (Cx (fn (t, f) => CJUMP (Tree.GE, ex1, ex2, t, f)))
                    | LE => unEx (Cx (fn (t, f) => CJUMP (Tree.LE, ex1, ex2, t, f)))
                    | AND => Tree.BINOP (Tree.AND, ex1, ex2)
end












