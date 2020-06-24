signature FRAME = sig
    type Offset
    type Frame
    val wordSize : Offset
    val allocVar : Frame -> Ast.id -> Frame * Tree.stm
    val allocInternalVar : Frame -> Frame * int * Tree.stm
    val getOffset : Frame -> Ast.id -> int option
    val emptyFrame : Frame
    
    val funcDecl : Ast.id list -> Frame
    val callFunction :  Temp.label -> Frame -> int list -> Tree.stm
    val getWordSize : int
end	

structure Frame :> FRAME = struct

    type Offset = int

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    datatype Frame = Frame of Offset * (Offset IdMap.map)

    val wordSize = 4

    val stackAllocStmt = Tree.SEQ (
        Tree.MOVE (Tree.argTemp1, Tree.CONST (~wordSize)),
        Tree.MOVE (Tree.stackTemp, Tree.BINOP (Tree.PLUS, Tree.stackTemp, Tree.argTemp1))
    )

    fun allocVar (Frame (currOffset, varMap)) var = 
        let
            val newMap = case IdMap.find (varMap, var) of
                NONE => IdMap.insert (varMap, var, currOffset)
                | SOME prevOffset =>
                    let
                        val (mapAfterRemove, _) = IdMap.remove (varMap, var)
                    in
                        IdMap.insert (mapAfterRemove, var, currOffset)
                    end

            val newOffset = currOffset - wordSize
        in
            (Frame (newOffset, newMap), stackAllocStmt)
        end

    fun allocInternalVar (Frame (currOffset, varMap)) = (Frame (currOffset - wordSize, varMap), currOffset, stackAllocStmt)

    fun getOffset (Frame (_, varMap)) var = IdMap.find (varMap, var)

    val emptyFrame = Frame (0, IdMap.empty)

    exception FunctionNotInScope

    fun getStaticLink 
        (funcMap : Func.FuncMap)
        (caller : Ast.id)
        (callee : Ast.id) : Tree.stm = 
        
        let
            val callerParent = case Func.find (funcMap, caller) of
                SOME func => func
                | NONE => raise FunctionNotInScope

            val calleeParent = case Func.find (funcMap, callee) of
                SOME func => func
                | NONE => raise FunctionNotInScope

            (*
                Search ancestors of caller to check if parent of callee is found,
                at the start of each step in recursion, resultTemp contains frame
                pointer value of func (i.e. static link of any child of func),
                we stop when parent of callee matches func
            *)
            fun searchAncestor prevStmList func =
                if (func = calleeParent) then
                    Tree.seq prevStmList
                else
                    let
                        val parentOfFunc = case Func.find (funcMap, func) of
                            SOME fnc => fnc
                            | NONE => raise FunctionNotInScope
                        
                        val newStmList = prevStmList
                            @ [
                                Tree.MOVE (
                                    Tree.resultTemp,
                                    Tree.MEM (
                                        Tree.BINOP (
                                            Tree.PLUS,
                                            Tree.resultTemp,
                                            Tree.CONST (~2 * wordSize)
                                        )
                                    )
                                )
                            ]
                    in
                        searchAncestor newStmList parentOfFunc
                    end
        in
            if (calleeParent = caller) then
                Tree.MOVE (Tree.resultTemp, Tree.frameTemp)
            else
                searchAncestor [] caller
        end

    fun funcDecl argNameList =
        let
            fun placeVarInMap (argName, currFrame) = 
                let
                    val Frame (currOffset, currMap) = currFrame
                in
                    Frame (currOffset - wordSize, IdMap.insert (currMap, argName, currOffset))
                end
        in
            foldl placeVarInMap (Frame (~3 * wordSize, IdMap.empty)) argNameList
        end

    fun callFunction funcLabel currFrame listOffset =
        let
            val numParam = List.length listOffset + 3
            val Frame (currOffset, _) = currFrame

            val storeFp = Tree.moveTempToFrame currOffset Tree.frameTemp

            fun foldFun (prevMemOffset, (curr_offset, currStmtList)) =
                let
                    val stmt = [
                        Tree.moveFrameToTemp Tree.argTemp1 prevMemOffset,
                        Tree.moveTempToFrame curr_offset Tree.argTemp1
                    ]
                in
                    (curr_offset - wordSize, currStmtList @ stmt)
                end

            
            val optStoreArgStmt = case listOffset of
                [] => NONE
                | list_offset =>
                    let
                        val (_, storeArgStmtList) =  
                            foldl foldFun (currOffset - 3 * wordSize, []) list_offset
                    in
                        SOME (Tree.seq storeArgStmtList)
                    end

            val updateFpSp = Tree.seq [
                Tree.MOVE (Tree.frameTemp, Tree.stackTemp),
                Tree.MOVE (Tree.argTemp1, Tree.CONST (~numParam * wordSize)),
                Tree.MOVE (Tree.stackTemp, Tree.BINOP (Tree.PLUS, Tree.stackTemp, Tree.argTemp1))
            ]

            val callFunc = Tree.EXP (Tree.CALL (Tree.NAME funcLabel, []))

            val restoreFpSp = Tree.seq [
                Tree.MOVE (Tree.stackTemp, Tree.frameTemp),
                Tree.moveFrameToTemp Tree.frameTemp 0
            ]

            val moveRetValToResult = Tree.MOVE (Tree.resultTemp, Tree.returnTemp)

        in
            case optStoreArgStmt of
                SOME storeArgStmt =>
                    Tree.seq [
                        storeFp,            (* Store frame pointer *)
                        storeArgStmt,       (* Store arguments *)
                        updateFpSp,         (* Update FP, SP *)
                        callFunc,           (* Call function *)
                        restoreFpSp,        (* Restore FP, SP *)
                        moveRetValToResult  (* Get return value in result *)
                    ]
                | NONE =>
                    Tree.seq [
                        storeFp,            (* Store frame pointer *)
                        updateFpSp,         (* Update FP, SP *)
                        callFunc,           (* Call function *)
                        restoreFpSp,        (* Restore FP, SP *)
                        moveRetValToResult  (* Get return value in result *)
                    ]
        end

        val getWordSize = wordSize
end