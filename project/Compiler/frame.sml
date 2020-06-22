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

    fun funcDecl argNameList =
        let
            fun placeVarInMap (argName, currFrame) = 
                let
                    val Frame (currOffset, currMap) = currFrame
                in
                    Frame (currOffset - wordSize, IdMap.insert (currMap, argName, currOffset))
                end
        in
            foldl placeVarInMap (Frame (~2 * wordSize, IdMap.empty)) argNameList
        end

    fun callFunction funcLabel currFrame listOffset =
        let
            val numParam = List.length listOffset + 2
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

            val (_, storeArgStmtList) =  
                foldl foldFun (currOffset - 2 * wordSize, []) listOffset
            val storeArgStmt = Tree.seq storeArgStmtList 

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
            Tree.seq [
                storeFp,            (* Store frame pointer *)
                storeArgStmt,       (* Store arguments *)
                updateFpSp,         (* Update FP, SP *)
                callFunc,           (* Call function *)
                restoreFpSp,        (* Restore FP, SP *)
                moveRetValToResult  (* Get return value in result *)
            ]
        end

        val getWordSize = wordSize
end