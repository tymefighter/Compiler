structure Translate = struct

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
    datatype Info = Info of (Temp.label option) * Frame.Frame

    fun getLoopLabel (Info (optLabel, _)) = optLabel
    fun getFrame (Info (_, frame))        = frame

    fun setLoopLabel (Info (_, frame)) newOptLabel = Info (newOptLabel, frame)
    fun setFrame (Info (optLabel, _))  newFrame    = Info (optLabel, newFrame)


    datatype exp = Ex of Tree.exp
        | Nx of Tree.stm
        | Cx of Temp.label * Temp.label -> Tree.stm

    fun getStmt (Tree.ESEQ (stmt, temp)) = stmt
        | getStmt _ = raise RestrictionFailed

    fun unEx (Ex e) = e
        | unEx (Nx s) =
            let
                val stmtAfterMoving = Tree.seq [
                    s,
                    Tree.MOVE (Tree.resultTemp, Tree.CONST 0)
                ]
            in
                Tree.ESEQ (stmtAfterMoving, Tree.resultTemp)
            end
        | unEx (Cx con) = let
                
                val t = Temp.newlabel ()  (* True then jump here *)
                val f = Temp.newlabel ()  (* False then jump here *)
                val cont = Temp.newlabel ()
            in
                Tree.ESEQ (Tree.seq [
                    con (t, f),
                    Tree.LABEL t,
                    Tree.MOVE (Tree.resultTemp, Tree.CONST 1),
                    Tree.JUMP (Tree.NAME cont, [cont]),
                    Tree.LABEL f,
                    Tree.MOVE (Tree.resultTemp, Tree.CONST 0),
                    Tree.LABEL cont
                ], Tree.resultTemp)
            end

    fun unNx (Ex e) = getStmt e
        | unNx (Nx s) = s
        | unNx (Cx con) = raise ConditionalToNoReturn
    
    fun unCx (Ex e) = (fn (t, f) => Tree.seq [
        getStmt e,
        Tree.MOVE (Tree.argTemp1, Tree.resultTemp),
        Tree.MOVE (Tree.argTemp2, Tree.CONST 0),
        Tree.CJUMP (Tree.NE, Tree.argTemp1, Tree.argTemp2, t, f)
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
   
    fun translateExp info (Ast.LiteralInt x) = (info, Ex (Tree.ESEQ (Tree.MOVE (Tree.resultTemp, Tree.CONST x), Tree.resultTemp)))
        | translateExp info (Ast.Op (e1, bin_op, e2)) = 
            let
                val (info1, transEx1) = translateExp info e1
                val ex1 = unEx transEx1
                val (frame1, exOffset1, allocStmt1) = Frame.allocInternalVar (getFrame info1)

                val (info2, transEx2) = translateExp (setFrame info1 frame1) e2
                val ex2 = unEx transEx2
                val (frame2, exOffset2, allocStmt2) = Frame.allocInternalVar (getFrame info2)

                val ex = case bin_op of
                    Ast.ADD => Tree.BINOP (Tree.PLUS, Tree.argTemp1, Tree.argTemp2)
                    | Ast.SUB => Tree.BINOP (Tree.MINUS, Tree.argTemp1, Tree.argTemp2)
                    | Ast.MUL => Tree.BINOP (Tree.MUL, Tree.argTemp1, Tree.argTemp2)
                    | Ast.DIV => Tree.BINOP (Tree.DIV, Tree.argTemp1, Tree.argTemp2)
                    | Ast.EQ => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.EQ, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.NE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.NE, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.G => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GT, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.L => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LT, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.GE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.GE, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.LE => unEx (Cx (fn (t, f) => Tree.CJUMP (Tree.LE, Tree.argTemp1, Tree.argTemp2, t, f)))
                    | Ast.AND => Tree.BINOP (Tree.AND, Tree.argTemp1, Tree.argTemp2)
                    | Ast.OR => Tree.BINOP (Tree.OR, Tree.argTemp1, Tree.argTemp2)

                val stmt = case ex of
                    Tree.ESEQ _ => getStmt ex
                    | _ => Tree.MOVE (Tree.resultTemp, ex)

                val computeAndMoveToTemp = Tree.seq [
                    getStmt ex1,
                    allocStmt1,
                    Tree.moveTempToFrame exOffset1 Tree.resultTemp,
                    getStmt ex2,
                    allocStmt2,
                    Tree.moveTempToFrame exOffset2 Tree.resultTemp,
                    Tree.moveFrameToTemp Tree.argTemp1 exOffset1,
                    Tree.moveFrameToTemp Tree.argTemp2 exOffset2,
                    stmt
                ]
            in
                (setFrame info2 frame2, Ex (Tree.ESEQ (computeAndMoveToTemp, Tree.resultTemp)))
            end
        | translateExp info (Ast.NegExp e) = 
            let
                val (newInfo, transEx) = translateExp info e
                val ex = unEx transEx
                val computeAndMoveToTemp = Tree.seq [
                    getStmt ex,
                    Tree.MOVE (Tree.argTemp1, Tree.CONST 0),
                    Tree.MOVE (Tree.resultTemp, Tree.BINOP (Tree.MINUS, Tree.argTemp1, Tree.resultTemp))
                ]
            in
                (newInfo, Ex (Tree.ESEQ (computeAndMoveToTemp, Tree.resultTemp)))
            end
        | translateExp info (Ast.Exprs exp_list) = let
                val (ls, last_ele) = seperateLastEle exp_list
                val (info1, stmtList) = translateList info ls
                
                val (info2, transEx) = translateExp info last_ele
                val ex = unEx transEx
            in
                (info2, Ex (Tree.ESEQ (Tree.seq (stmtList @ [getStmt ex]), Tree.resultTemp)))
            end
        | translateExp info (Ast.IfThen (cond_e, e)) = let
                val (info1, transConEx) = translateExp info cond_e
                val cnd = unCx transConEx
                val (info2, transEx) = translateExp info1 e
                val ex = unEx transEx

                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val stmts = Tree.seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    getStmt ex,
                    Tree.LABEL false_label
                ]
            in
                (info2, Nx stmts)
            end
        | translateExp info (Ast.IfThenElse (cond_e, true_e, false_e)) = let
                val (info1, transConEx) = translateExp info cond_e
                val cnd = unCx transConEx
                val (info2, transTrueEx) = translateExp info1 true_e
                val trueEx = unEx transTrueEx
                val (info3, transFalseEx) = translateExp info2 false_e
                val falseEx = unEx transFalseEx

                val true_label = Temp.newlabel ()
                val false_label = Temp.newlabel ()
                val continue_label = Temp.newlabel ()
                val stmts = Tree.seq [
                    cnd (true_label, false_label),
                    Tree.LABEL true_label,
                    getStmt trueEx,
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL false_label,
                    getStmt falseEx,
                    Tree.JUMP (Tree.NAME continue_label, [continue_label]),
                    Tree.LABEL continue_label
                ]
            in
                (info3, Nx stmts)
            end
        | translateExp info (Ast.While (cond_e, exp)) = let
                val (info1, transConEx) = translateExp info cond_e
                val cnd = unCx transConEx
                val (info2, transEx) = translateExp info1 exp
                val ex = unEx transEx

                val loop_label = Temp.newlabel ()
                val true_label = Temp.newlabel ()
                val end_label = Temp.newlabel ()

                val newInfo = setLoopLabel info (SOME end_label)
                val stmts = Tree.seq [
                    Tree.LABEL loop_label,
                    cnd (true_label, end_label),
                    Tree.LABEL true_label,
                    getStmt ex,
                    Tree.JUMP (Tree.NAME loop_label, [loop_label]),
                    Tree.LABEL end_label
                ]
            in
                (info2, Nx stmts)
            end
        | translateExp info (Ast.For (var, start_exp, end_exp, body_exp)) = let
                val initFrame = getFrame info

                val (nextFrame, allocStmt1) = Frame.allocVar initFrame var
                val (finalFrame, endExpOffset, allocStmt2) = Frame.allocInternalVar nextFrame
                val info1 = setFrame info finalFrame

                val varOffset = case Frame.getOffset finalFrame var of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val loop_label = Temp.newlabel ()
                val continue_label = Temp.newlabel ()

                val (info2, startEx) = translateExp info1 start_exp
                val startExp = unEx startEx
                val (info3, endEx) = translateExp info2 end_exp
                val endExp = unEx endEx
                val (info4, bodyEx) = translateExp info3 body_exp
                val bodyExp = unEx bodyEx

                val stmts = Tree.seq [
                    allocStmt1,
                    allocStmt2,
                    getStmt startExp,
                    Tree.moveTempToFrame varOffset Tree.resultTemp,
                    getStmt endExp,
                    Tree.moveTempToFrame endExpOffset Tree.resultTemp,
                    Tree.LABEL loop_label,
                    getStmt bodyExp,
                    Tree.moveFrameToTemp Tree.argTemp1 varOffset,
                    Tree.MOVE (Tree.argTemp2, Tree.CONST 1),
                    Tree.MOVE (Tree.argTemp1, Tree.BINOP (Tree.PLUS, Tree.argTemp1, Tree.argTemp2)),
                    Tree.moveTempToFrame varOffset Tree.argTemp1,
                    Tree.moveFrameToTemp Tree.argTemp2 endExpOffset,
                    Tree.CJUMP (Tree.EQ, Tree.argTemp1, Tree.argTemp2, continue_label, loop_label),
                    Tree.LABEL continue_label
                ]
            in
                (info4, Nx stmts)
            end
        | translateExp info Ast.Break = (case getLoopLabel info of
            NONE => raise BreakUsedIncorrectly
            | SOME loopLabel => (info, Nx (Tree.JUMP (Tree.NAME loopLabel, [loopLabel]))))

        | translateExp info (Ast.Lval (Ast.Var var)) = let
                val frame = getFrame info
                val optVarOffset = Frame.getOffset frame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

            in
                (info, Ex (Tree.ESEQ (Tree.moveFrameToTemp Tree.resultTemp varOffset, Tree.resultTemp)))
            end
        | translateExp info (Ast.Assignment (Ast.Var var, e)) = let
                val (info1, transEx) = translateExp info e
                val ex = unEx transEx

                val frame = getFrame info1
                val optVarOffset = Frame.getOffset frame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val stmt = Tree.seq [
                    getStmt ex,
                    Tree.moveTempToFrame varOffset Tree.resultTemp
                ]
            in
                (info1, Nx stmt)
            end
        (* Empty Let block would give empty sequence error *)
        | translateExp info (Ast.LetStmt (dec_list, exp_list)) = let
                val (info1, stmt_nx) = addDec info [] dec_list   (* Many Changes here !!!!*)
                val stmtLet = unNx stmt_nx

                val (ls, last_ele) = seperateLastEle exp_list
                val (info2, stmtExpList) = translateList info1 ls
                val new_stmt = Tree.seq (stmtLet :: stmtExpList)

                val (info3, transEx) = translateExp info2 last_ele
                val ex = unEx transEx

                val complete_stmt = Tree.seq [
                    new_stmt,
                    getStmt ex
                ]
            in
                (info3, Ex (Tree.ESEQ (complete_stmt, Tree.resultTemp)))
            end
        | translateExp info (Ast.FunCall (funcName, args)) =
            if(funcName = "print" orelse funcName = "println")
                then
                    let
                        val exp = case args of
                            [arg] => arg
                            | _ => raise RestrictionFailed
                        val (newInfo, transEx) = translateExp info exp
                        val ex = unEx transEx
                        val stmt = Tree.seq [
                            getStmt ex,
                            Tree.EXP (Tree.CALL (Tree.NAME (Temp.stringToLabel funcName), [Tree.resultTemp]))
                        ]
                    in
                        (newInfo, Nx stmt)
                    end
            else raise Unimplemeneted
        | translateExp _ _ = raise Unimplemeneted

    and translateDec info (Ast.Vardec (var, var_type_opt, e)) = 
            let
                val frame = getFrame info
                val (newFrame, allocStmt) = Frame.allocVar frame var
                
                val optVarOffset = Frame.getOffset newFrame var
                val varOffset = case optVarOffset of
                    NONE => raise VariableUsedBeforeDec
                    | SOME var_offset => var_offset

                val (newInfo, transEx) = translateExp (setFrame info newFrame) e
                val ex = unEx transEx

                val stmts = Tree.seq [
                    allocStmt,
                    getStmt ex,
                    Tree.moveTempToFrame varOffset Tree.resultTemp
                ]
            in
                (
                    newInfo,
                    Nx stmts
                )
            end
        | translateDec _ (Ast.FuncDec (funcName, argAndTypeList, funcType, funcExp)) = 
            let

                val placeLabel = Tree.LABEL (Temp.stringToLabel funcName)

                fun getName (argName, _) = argName
                val argNameList = map getName argAndTypeList

                val info = Info (NONE, Frame.funcDecl argNameList)

                val returnAddrOffset = 2
                val saveReturnAddr =
                    Tree.moveTempToFrame returnAddrOffset Tree.returnAddrTemp
                
                val (newInfo, transFunEx) = translateExp info funcExp
                val funcStmt = getStmt (unEx transFunEx)
                val placeRetValue = Tree.MOVE (Tree.returnTemp, Tree.resultTemp)

                val moveReturnAddr = Tree.moveFrameToTemp Tree.returnAddrTemp returnAddrOffset

                val jumpToCaller = Tree.JUMP (Tree.returnAddrTemp, [])

                val stmt = Tree.seq [
                    placeLabel,     (* Place function label *)
                    saveReturnAddr, (* Save return address *)
                    funcStmt,       (* Function body *)
                    placeRetValue,  (* Place return value *)
                    moveReturnAddr, (* Move return address to return address temp*)
                    jumpToCaller    (* Return back to caller *)
                ]
            in
                (
                    newInfo,
                    Nx stmt
                )
            end
        | translateDec _ _ = raise Unimplemeneted

    and translateList info astExpList = 
        let
            val initAccum = (info, [])
            fun foldFunc (currExp, (prevInfo, prevStmtList)) = 
                let
                    val (currInfo, transEx) = translateExp prevInfo currExp
                    val currStmt = unNx transEx
                in
                    (currInfo, prevStmtList @ [currStmt])
                end
        in
            foldl foldFunc initAccum astExpList
        end

    and addDec prev_info prev_stmts (dec :: dec_ls) = let
                val (newInfo, stmt_nx) = translateDec prev_info dec
            in
                addDec newInfo (prev_stmts @ [unNx stmt_nx]) dec_ls
            end
        | addDec finalInfo final_stmt_list [] = (finalInfo, Nx (Tree.seq final_stmt_list))

    fun translateProg (Ast.Expression exp) = 
            let
                val (_, transEx) = translateExp (Info (NONE, Frame.emptyFrame)) exp
            in
                unEx transEx
            end
        
        | translateProg (Ast.Decs dec_list) = 
            let
                val (_, stmt_list) = addDec (Info (NONE, Frame.emptyFrame)) [] dec_list
            in
                unEx stmt_list
            end
end