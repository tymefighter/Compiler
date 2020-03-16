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
    
    exception Incomplete

    fun pprintRelop EQ = "EQ"
        | pprintRelop NE = "NE"
        | pprintRelop LT = "LT"
        | pprintRelop GT = "GT"
        | pprintRelop LE = "LE"
        | pprintRelop GE = "GE"
        | pprintRelop ULT = "ULT"
        | pprintRelop ULE = "ULE"
        | pprintRelop UGT = "UGT"
        | pprintRelop UGE = "UGE"

    fun pprintExp (CONST n) = Int.toString n
        | pprintExp (NAME lab) = "NAME " ^ lab
        | pprintExp (TEMP tmp) = "Temp " ^ tmp
        | pprintExp (BINOP (bin_op, e1, e2)) = let
                val bin_op_str = case bin_op of 
                    PLUS => "PLUS "
                    | MINUS => "MINUS "
                    | MUL => "MUL "
                    | DIV => "DIV "
                    | AND => "AND "
                    | OR => "OR "
                    | LSHIFT => "LSHIFT "
                    | RSHIFT => "RSHIFT "
                    | ARSHIFT => "ARSHIFT "
                    | XOR => "XOR "
            in
                bin_op_str ^ "(" ^ pprintExp e1 ^ ", " ^ pprintExp e2 ^ ")"
            end
        | pprintExp (MEM e) = "Mem (" ^ pprintExp e ^ ")"
        | pprintExp (CALL (f_name, arg_list)) = let
                fun printArg [arg] = pprintExp arg
                    | printArg (arg :: arg_ls) = pprintExp arg ^ ", " ^ printArg arg_ls
                    | printArg [] = ""
            in
                pprintExp f_name ^ " (" ^ printArg arg_list ^ ")"
            end
        | pprintExp (ESEQ (st, ex)) = "ESEQ (" ^ pprintStm st ^ ", " ^ pprintExp ex ^ ")"
    
    and pprintStm (MOVE (to, value)) = "MOVE (" ^ pprintExp to ^ ", " ^ pprintExp value ^ ")"
        | pprintStm (EXP ex) = "EXP (" ^ pprintExp ex ^ ")"
        | pprintStm (JUMP (loc, _)) = "JUMP (" ^ pprintExp loc ^ ")"
        | pprintStm (CJUMP (rel_op, e1, e2, true_lab, false_lab)) = "CJUMP (" ^ pprintRelop rel_op ^ ", " ^ pprintExp e1 ^ ", " ^ pprintExp e2 ^ ", " ^ true_lab ^ ", " ^ false_lab ^ ")" 
        | pprintStm (SEQ (st1, st2)) = "SEQ (" ^ pprintStm st1 ^ ", " ^ pprintStm st2 ^ ")"
        | pprintStm (LABEL lab) = "Label (" ^ lab ^ ")"
end

structure Env = struct

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    type env = Temp.temp IdMap.map
    
    fun newEnv env ((var, temp) :: var_temp_ls) = newEnv (IdMap.insert (env, var, temp)) var_temp_ls
        | newEnv env [] = env

end

structure Translate = struct

    exception EmptySeq
    exception EmptyExpressionList
    exception ConditionalToNoReturn
    exception NoReturnToConditional

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
                    Tree.LABEL f,
                    Tree.MOVE (Tree.TEMP res, Tree.CONST 0),
                    Tree.LABEL t
                ], Tree.TEMP res)
            end

    fun unNx (Ex e) = Tree.EXP e
        | unNx (Nx s) = s
        | unNx (Cx con) = raise ConditionalToNoReturn
    
    fun unCx (Ex e) = (fn (t, f) => Tree.CJUMP (Tree.NE, e, Tree.CONST 0, t, f))
        | unCx (Nx s) = raise NoReturnToConditional
        | unCx (Cx c) = c
   
     fun translateExp (Ast.LiteralInt x) = Ex (Tree.CONST x)
        | translateExp (Ast.Op (e1, bin_op, e2)) = let
                val ex1 = unEx (translateExp e1)
                val ex2 = unEx (translateExp e2)
                val ex = case bin_op of
                    Ast.ADD => Tree.BINOP (Tree.PLUS, ex1, ex2)
                    | Ast.SUB => Tree.BINOP (Tree.MINUS, ex1, ex2)
                    | Ast.MUL => Tree.BINOP (Tree.MUL, ex1, ex2)
                    | Ast.DIV => Tree.BINOP (Tree.DIV, ex1, ex2)
                    | Ast.EQ => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.EQ, ex1, ex2, t, f)))
                    | Ast.NE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.NE, ex1, ex2, t, f)))
                    | Ast.G => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GT, ex1, ex2, t, f)))
                    | Ast.L => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LT, ex1, ex2, t, f)))
                    | Ast.GE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GE, ex1, ex2, t, f)))
                    | Ast.LE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LE, ex1, ex2, t, f)))
                    | Ast.AND => Tree.BINOP (Tree.AND, ex1, ex2)
                    | Ast.OR => Tree.BINOP (Tree.OR, ex1, ex2)
            in
                Ex ex
            end
        | translateExp (Ast.NegExp e) = Ex (Tree.BINOP (Tree.MINUS, Tree.CONST 0, unEx (translateExp e)))
        | translateExp (Ast.Exprs exp_list) = let
                fun seperateLastEle [] = raise EmptyExpressionList
                    | seperateLastEle [x] = ([], x)
                    | seperateLastEle (x :: xs) = let
                            val (ls, last_ele) = seperateLastEle xs
                        in
                            (x :: ls, last_ele)
                        end
                val (ls, last_ele) = seperateLastEle exp_list
            in
                case ls of
                    [] => translateExp last_ele
                    | _ => Ex (Tree.ESEQ ((seq o map (unNx o translateExp)) ls, unEx (translateExp last_ele)))
            end
        | translateExp (Ast.IfThen (cond_ex, ex)) = let
                val cnd = unCx (translateExp cond_ex)
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val stmts = seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o translateExp) ex),
                    Tree.LABEL false_label
                ]
            in
                Nx stmts
            end
        | translateExp (Ast.IfThenElse (cond_ex, true_ex, false_ex)) = let
                val cnd = unCx (translateExp cond_ex)
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val continue_label = Temp.newlabel ()
                val stmts = seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o translateExp) true_ex),
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL false_label,
                    Tree.EXP ((unEx o translateExp) false_ex),
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL continue_label
                ]
            in
                Nx stmts
            end
        | translateExp (Ast.While (cond_exp, exp)) = let
                val cnd = unCx (translateExp cond_exp)
                val loop_label = Temp.newlabel ()
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val stmts = seq [
                    Tree.LABEL loop_label,
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o translateExp) exp),
                    Tree.JUMP (Tree.NAME loop_label, [loop_label]),
                    Tree.LABEL false_label
                ]
            in
                Nx stmts
            end
        | translateExp _ = Ex (Tree.CONST ~1)

    fun translateProg (Ast.Expression exp) = unEx (translateExp exp)
        | translateProg _ = (Tree.CONST ~1)
end
