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
        | pprintExp (NAME lab) = "NAME " ^ Temp.labelToString lab
        | pprintExp (TEMP tmp) = "Temp " ^ Temp.tempToString tmp
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
        | pprintStm (CJUMP (rel_op, e1, e2, true_lab, false_lab)) = "CJUMP (" ^ pprintRelop rel_op ^ ", " ^ pprintExp e1 ^ ", " ^ pprintExp e2 ^ ", " ^ Temp.labelToString true_lab ^ ", " ^ Temp.labelToString false_lab ^ ")" 
        | pprintStm (SEQ (st1, st2)) = "SEQ (" ^ pprintStm st1 ^ ", " ^ pprintStm st2 ^ ")"
        | pprintStm (LABEL lab) = "Label (" ^ Temp.labelToString lab ^ ")"
end

structure Env = struct

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    type environment = Temp.temp IdMap.map
    
    fun newEnv env ((var, temp) :: var_temp_ls) = newEnv (IdMap.insert (env, var, temp)) var_temp_ls
        | newEnv env [] = env
    
    val emptyEnv = IdMap.empty
    val findVar = IdMap.find

end

structure Translate = struct

    exception EmptySeq
    exception EmptyExpressionList
    exception ConditionalToNoReturn
    exception NoReturnToConditional
    exception BreakUsedIncorrectly
    exception VariableUsedBeforeDec

    (* 
        Temp.label: This stores the end of loop label of the loop just above in hierarchy 
        Env.environment: Variable to temporary mapping
    *)
    datatype Info = Info of (Temp.label option) * Env.environment

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
   
     fun translateExp info (Ast.LiteralInt x) = Ex (Tree.CONST x)
        | translateExp info (Ast.Op (e1, bin_op, e2)) = let
                val ex1 = unEx (translateExp info e1)
                val ex2 = unEx (translateExp info e2)
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
        | translateExp info (Ast.NegExp e) = Ex (Tree.BINOP (Tree.MINUS, Tree.CONST 0, unEx (translateExp info e)))
        | translateExp info (Ast.Exprs exp_list) = let
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
                    [] => translateExp info last_ele
                    | _ => Ex (Tree.ESEQ ((seq o map (unNx o translateExp info)) ls, unEx (translateExp info last_ele)))
            end
        | translateExp info (Ast.IfThen (cond_ex, ex)) = let
                val cnd = unCx (translateExp info cond_ex)
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val stmts = seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o (translateExp info)) ex),
                    Tree.LABEL false_label
                ]
            in
                Nx stmts
            end
        | translateExp info (Ast.IfThenElse (cond_ex, true_ex, false_ex)) = let
                val cnd = unCx (translateExp info cond_ex)
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val continue_label = Temp.newlabel ()
                val stmts = seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o translateExp info) true_ex),
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL false_label,
                    Tree.EXP ((unEx o translateExp info) false_ex),
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL continue_label
                ]
            in
                Nx stmts
            end
        | translateExp info (Ast.While (cond_exp, exp)) = let
                val cnd = unCx (translateExp info cond_exp)
                val loop_label = Temp.newlabel ()
                val true_label = Temp.newlabel ()
                val end_label = Temp.newlabel ()
                val (Info (_, env)) = info
                val new_info = Info (SOME end_label, env) 
                val stmts = seq [
                    Tree.LABEL loop_label,
                    cnd (true_label, end_label),
                    Tree.LABEL true_label,
                    Tree.EXP ((unEx o translateExp new_info) exp),
                    Tree.JUMP (Tree.NAME loop_label, [loop_label]),
                    Tree.LABEL end_label
                ]
            in
                Nx stmts
            end
        | translateExp info (Ast.For (var, start_exp, end_exp, body_exp)) = let
                val (Info (_, env)) = info
                val loop_temp = Temp.newtemp ()
                val new_env = Env.newEnv env [(var, loop_temp)]
                
                val start_ex = unEx (translateExp info start_exp)
                val end_ex = unEx (translateExp info end_exp)

                val loop_label = Temp.newlabel ()
                val cont_label = Temp.newlabel ()
                val end_label = Temp.newlabel ()
                val eval_endex_temp = Temp.newtemp ()

                val new_info = Info (SOME end_label, new_env)
                val body_stmt = unNx (translateExp new_info body_exp)
                
                val stmts = seq [
                    Tree.MOVE (Tree.TEMP eval_endex_temp, end_ex),
                    Tree.MOVE (Tree.TEMP loop_temp, start_ex),
                    Tree.LABEL loop_label,
                    Tree.CJUMP (Tree.NE, Tree.TEMP loop_temp, Tree.TEMP eval_endex_temp, cont_label, end_label),
                    Tree.LABEL cont_label,
                    body_stmt,
                    Tree.MOVE (Tree.TEMP loop_temp, Tree.BINOP (Tree.PLUS, Tree.TEMP loop_temp, Tree.CONST 1)),
                    Tree.JUMP (Tree.NAME loop_label, [loop_label]),
                    Tree.LABEL end_label
                ]
            in
                Nx stmts
            end
        | translateExp (Info (break_label_opt, _)) Ast.Break = (
            case break_label_opt of
                NONE => raise BreakUsedIncorrectly
                | SOME break_label => Nx (Tree.JUMP (Tree.NAME break_label, [break_label]))
        )
        | translateExp (Info (_, env)) (Ast.Lval (Ast.Var var)) = let
                val var_temp_opt = Env.findVar (env, var)
            in
                case var_temp_opt of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_temp => Ex (Tree.TEMP var_temp)
            end
        | translateExp info (Ast.Assignment (Ast.Var var, ex)) = let
                val e = unEx (translateExp info ex)
                val (Info (_, env)) = info
                val var_temp_opt = Env.findVar (env, var)
                val var_temp = case var_temp_opt of
                    NONE => raise VariableUsedBeforeDec
                    | SOME vt => vt
            in
                Nx (Tree.MOVE (Tree.TEMP var_temp, e))
            end
        | translateExp _ _ = Ex (Tree.CONST ~1)

    fun translateDec info (Ast.Vardec (var, var_type_opt, ex)) = let
                val e = unEx (translateExp info ex)
                val (Info (_, env)) = info
                val new_var_temp = Temp.newtemp ()
                val new_env = Env.newEnv env [(var, new_var_temp)]
            in
                (
                    new_env,
                    Nx (Tree.MOVE (Tree.TEMP new_var_temp, e))
                )
            end
        | translateDec _ _ = (Env.emptyEnv, Ex (Tree.CONST ~1))

    fun translateProg (Ast.Expression exp) = unEx (translateExp (Info (NONE, Env.emptyEnv)) exp)
        | translateProg (Ast.Decs dec_list) = (let
                fun addDec prev_info prev_stmts (dec :: dec_ls) = let
                            val (new_env, stmt_nx) = translateDec prev_info dec
                            val (Info (break_label_opt, _)) = prev_info
                            val new_info = Info (break_label_opt, new_env)
                        in
                            addDec new_info (prev_stmts @ [unNx stmt_nx]) dec_ls
                        end
                    | addDec _ final_stmt_list [] = Nx (seq final_stmt_list)
            in
                unEx (addDec (Info (NONE, Env.emptyEnv)) [] dec_list)
            end)
end
