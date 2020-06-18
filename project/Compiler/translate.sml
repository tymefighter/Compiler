structure Translate = struct

    exception EmptySeq
    exception EmptyExpressionList
    exception ConditionalToNoReturn
    exception NoReturnToConditional
    exception BreakUsedIncorrectly
    exception VariableUsedBeforeDec
    exception Unimplemeneted
    exception RestrictionFailed


    (*
        Function list: information about all previously declared functions
        Each element -> (function name, function label, number of arguments)
    *)
    type functionlist = (Ast.id * Temp.label * int) list
    (*
        Data Structure to store information which would be required while
        translation
    *)
    datatype Info = Info of (Temp.label option) * Frame.Frame * functionlist

    fun getLoopLabel (Info (optLabel, _, _)) = optLabel
    fun getFrame (Info (_, frame, _))        = frame
    fun getFuncList (Info (_, _, funcList))  = funcList

    fun setLoopLabel (Info (_, frame, funcList)) newOptLabel = Info (newOptLabel, frame, funcList)
    fun setFrame (Info (optLabel, _, funcList))  newFrame    = Info (optLabel, newFrame, funcList)
    fun setFuncList (Info (optLabel, frame, _))  newFuncList = Info (optLabel, frame, newFuncList)

    val argTemp1 = Tree.TEMP Temp.argTemp1
    val argTemp2 = Tree.TEMP Temp.argTemp2
    val resultTemp = Tree.TEMP Temp.resultTemp
    val frameTemp = Tree.TEMP Temp.framePointer
    val stackTemp = Tree.TEMP Temp.stackPointer

    fun moveTempToFrame var_offset temp = Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, frameTemp, Tree.CONST var_offset)), temp)
    fun moveFrameToTemp temp var_offset = Tree.MOVE (temp, Tree.MEM (Tree.BINOP (Tree.PLUS, frameTemp, Tree.CONST var_offset)))

    datatype exp = Ex of Tree.exp
        | Nx of Tree.stm
        | Cx of Temp.label * Temp.label -> Tree.stm

    fun seq [st] = st
        | seq (st :: st_list) = Tree.SEQ (st, seq st_list)
        | seq [] = raise EmptySeq

    fun getStmt (Tree.ESEQ (stmt, temp)) = stmt
        | getStmt _ = raise RestrictionFailed

    fun unEx (Ex e) = e
        | unEx (Nx s) =
            let
                val stmtAfterMoving = seq [
                    s,
                    Tree.MOVE (resultTemp, Tree.CONST 0)
                ]
            in
                Tree.ESEQ (stmtAfterMoving, resultTemp)
            end
        | unEx (Cx con) = let
                
                val t = Temp.newlabel ()  (* True then jump here *)
                val f = Temp.newlabel ()  (* False then jump here *)

            in
                Tree.ESEQ (seq [
                    Tree.MOVE (resultTemp, Tree.CONST 1),
                    con (t, f),
                    Tree.LABEL f,
                    Tree.MOVE (resultTemp, Tree.CONST 0),
                    Tree.LABEL t
                ], resultTemp)
            end

    fun unNx (Ex e) = getStmt e
        | unNx (Nx s) = s
        | unNx (Cx con) = raise ConditionalToNoReturn
    
    fun unCx (Ex e) = (fn (t, f) => seq [
        getStmt e,
        Tree.MOVE (argTemp1, resultTemp),
        Tree.MOVE (argTemp2, Tree.CONST 0),
        Tree.CJUMP (Tree.NE, argTemp1, argTemp2, t, f)
    ])
        | unCx (Nx s) = raise NoReturnToConditional
        | unCx (Cx c) = c

    fun seperateLastEle [] = raise EmptyExpressionList
        | seperateLastEle [x] = ([], x)
        | seperateLastEle (x :: xs) = let
                val (ls, last_ele) = seperateLastEle xs
            in
                (x :: ls, last_ele)
            end
   
     fun translateExp info (Ast.LiteralInt x) = Ex (Tree.ESEQ (Tree.MOVE (resultTemp, Tree.CONST x), resultTemp))
        | translateExp info (Ast.Op (e1, bin_op, e2)) = 
            let
                val ex1 = unEx (translateExp info e1)
                val ex2 = unEx (translateExp info e2)

                val ex = case bin_op of
                    Ast.ADD => Tree.BINOP (Tree.PLUS, argTemp1, argTemp2)
                    | Ast.SUB => Tree.BINOP (Tree.MINUS, argTemp1, argTemp2)
                    | Ast.MUL => Tree.BINOP (Tree.MUL, argTemp1, argTemp2)
                    | Ast.DIV => Tree.BINOP (Tree.DIV, argTemp1, argTemp2)
                    | Ast.EQ => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.EQ, argTemp1, argTemp2, t, f)))
                    | Ast.NE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.NE, argTemp1, argTemp2, t, f)))
                    | Ast.G => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GT, argTemp1, argTemp2, t, f)))
                    | Ast.L => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LT, argTemp1, argTemp2, t, f)))
                    | Ast.GE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GE, argTemp1, argTemp2, t, f)))
                    | Ast.LE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LE, argTemp1, argTemp2, t, f)))
                    | Ast.AND => Tree.BINOP (Tree.AND, argTemp1, argTemp2)
                    | Ast.OR => Tree.BINOP (Tree.OR, argTemp1, argTemp2)

                val stmt = case ex of
                    Tree.ESEQ _ => getStmt ex
                    | _ => Tree.MOVE (resultTemp, ex)

                val frame               = getFrame info
                val (frame1, exOffset1) = Frame.allocInternalVar frame
                val (frame2, exOffset2) = Frame.allocInternalVar frame1

                val computeAndMoveToTemp = seq [
                    getStmt ex1,
                    moveTempToFrame exOffset1 resultTemp,
                    getStmt ex2,
                    moveTempToFrame exOffset2 resultTemp,
                    moveFrameToTemp argTemp1 exOffset1,
                    moveFrameToTemp argTemp2 exOffset2,
                    stmt
                ]
            in
                Ex (Tree.ESEQ (computeAndMoveToTemp, resultTemp))
            end
        | translateExp info (Ast.NegExp e) = 
            let
                val ex = unEx (translateExp info e)
                val computeAndMoveToTemp = seq [
                    getStmt ex,
                    Tree.MOVE (argTemp1, Tree.CONST 0),
                    Tree.MOVE (resultTemp, Tree.BINOP (Tree.MINUS, argTemp1, resultTemp))
                ]
            in
                Ex (Tree.ESEQ (computeAndMoveToTemp, resultTemp))
            end
        | translateExp info (Ast.Exprs exp_list) = let
                val (ls, last_ele) = seperateLastEle exp_list
                val ex = unEx (translateExp info last_ele)
            in
                case ls of
                    [] => Ex ex
                    | _ => 
                        let
                            val listStmts = (seq o map (unNx o (translateExp info))) ls
                            val evalAndMoveStmt = getStmt ex
                            val stmts = seq [
                                listStmts,
                                evalAndMoveStmt
                            ]
                        in
                            Ex (Tree.ESEQ (stmts, resultTemp))
                        end
            end
        | translateExp info (Ast.IfThen (cond_ex, ex)) = let
                val cnd = unCx (translateExp info cond_ex)
                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val stmts = seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    getStmt ((unEx o (translateExp info)) ex),
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
                    getStmt ((unEx o translateExp info) true_ex),
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL false_label,
                    getStmt ((unEx o translateExp info) false_ex),
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

                val newInfo = setLoopLabel info (SOME end_label)
                val stmts = seq [
                    Tree.LABEL loop_label,
                    cnd (true_label, end_label),
                    Tree.LABEL true_label,
                    getStmt ((unEx o translateExp newInfo) exp),
                    Tree.JUMP (Tree.NAME loop_label, [loop_label]),
                    Tree.LABEL end_label
                ]
            in
                Nx stmts
            end
        | translateExp info (Ast.For (var, start_exp, end_exp, body_exp)) = let
                val initFrame = getFrame info

                val nextFrame = Frame.allocVar initFrame var
                val (finalFrame, endExpOffset) = Frame.allocInternalVar nextFrame

                val varOffset = case Frame.getOffset finalFrame var of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val loop_label = Temp.newlabel ()
                val continue_label = Temp.newlabel ()

                val startExp = (unEx o translateExp info) start_exp
                val endExp = (unEx o translateExp info) end_exp

                val stmts = seq [
                    getStmt startExp,
                    moveTempToFrame varOffset resultTemp,
                    getStmt endExp,
                    moveTempToFrame endExpOffset resultTemp,
                    Tree.LABEL loop_label,
                    getStmt ((unEx o translateExp info) body_exp),
                    moveTempToFrame varOffset resultTemp,
                    Tree.MOVE (argTemp2, Tree.CONST 1),
                    Tree.MOVE (argTemp1, Tree.BINOP (Tree.PLUS, argTemp1, argTemp2)),
                    moveTempToFrame varOffset argTemp1,
                    moveFrameToTemp argTemp2 endExpOffset,
                    Tree.CJUMP (Tree.EQ, argTemp1, argTemp2, continue_label, loop_label),
                    Tree.LABEL continue_label
                ]
            in
                Nx stmts
            end
        | translateExp info Ast.Break = (case getLoopLabel info of
            NONE => raise BreakUsedIncorrectly
            | SOME loopLabel => Nx (Tree.JUMP (Tree.NAME loopLabel, [loopLabel])))

        | translateExp info (Ast.Lval (Ast.Var var)) = let
                val frame = getFrame info
                val optVarOffset = Frame.getOffset frame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

            in
                Ex (Tree.ESEQ (moveFrameToTemp resultTemp varOffset, resultTemp))
            end
        | translateExp info (Ast.Assignment (Ast.Var var, e)) = let
                val ex = unEx (translateExp info e)
                val frame = getFrame info
                val optVarOffset = Frame.getOffset frame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val stmt = seq [
                    getStmt ex,
                    moveTempToFrame varOffset resultTemp
                ]
            in
                Nx stmt
            end
        (* Empty Let block would give empty sequence error *)
        | translateExp info (Ast.LetStmt (dec_list, exp_list)) = let 
                val (newInfo, stmt_nx) = addDec info [] dec_list
                val stmt = unNx stmt_nx
                val (ls, last_ele) = seperateLastEle exp_list
                val new_stmt = seq (stmt :: map (unNx o translateExp newInfo) ls)
                val complete_stmt = seq [
                    new_stmt,
                    getStmt (unEx (translateExp newInfo last_ele))
                ]
            in
                Ex (Tree.ESEQ (complete_stmt, resultTemp))
            end
        | translateExp _ _ = raise Unimplemeneted

    and translateDec info (Ast.Vardec (var, var_type_opt, e)) = let
                val ex = unEx (translateExp info e)
                val frame = getFrame info
                val newFrame = Frame.allocVar frame var
                
                val optVarOffset = Frame.getOffset frame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val stmts = seq [
                    getStmt ex,
                    moveTempToFrame varOffset resultTemp
                ]
            in
                (
                    setFrame info newFrame,
                    Nx stmts
                )
            end
        | translateDec _ _ = raise Unimplemeneted

    and addDec prev_info prev_stmts (dec :: dec_ls) = let
                val (newInfo, stmt_nx) = translateDec prev_info dec
            in
                addDec newInfo (prev_stmts @ [unNx stmt_nx]) dec_ls
            end
        | addDec finalInfo final_stmt_list [] = (finalInfo, Nx (seq final_stmt_list))

    fun translateProg (Ast.Expression exp) = unEx (translateExp (Info (NONE, Frame.emptyFrame, [])) exp)
        | translateProg (Ast.Decs dec_list) = (let
                val (_, stmt_list) = addDec (Info (NONE, Frame.emptyFrame, [])) [] dec_list
            in
                unEx stmt_list
            end)
end