signature FRAME = sig
    type Offset
    type Frame
    val wordSize : Offset
    val allocVar : Frame -> Ast.id -> Frame * Tree.stm
    val allocInternalVar : Frame -> Frame * int * Tree.stm
    val getOffset : Frame -> Ast.id -> int option
    val emptyFrame : Frame

    val callFunction :  Temp.label -> Frame -> int list -> Tree.stm
end	

structure Frame :> FRAME = struct

    type Offset = int

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    datatype Frame = Frame of Offset * (Offset IdMap.map)

    val wordSize = 4

    val argTemp = Tree.TEMP Temp.argTemp1
    val stackTemp = Tree.TEMP Temp.stackPointer
    val frameTemp = Tree.TEMP Temp.framePointer
    val returnTemp = Tree.TEMP Temp.returnValue
    val resultTemp = Tree.TEMP Temp.resultTemp

    val stackAllocStmt = Tree.SEQ(
        Tree.MOVE (argTemp, Tree.CONST (~wordSize)),
        Tree.MOVE (stackTemp, Tree.BINOP (Tree.PLUS, stackTemp, argTemp))
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

            val newOffset = currOffset + wordSize
        in
            (Frame (newOffset, newMap), stackAllocStmt)
        end

    fun allocInternalVar (Frame (currOffset, varMap)) = (Frame (currOffset + wordSize, varMap), currOffset, stackAllocStmt)

    fun getOffset (Frame (_, varMap)) var = IdMap.find (varMap, var)

    val emptyFrame = Frame (0, IdMap.empty)

    fun moveTempToFrame var_offset temp = Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, frameTemp, Tree.CONST var_offset)), temp)
    fun moveFrameToTemp temp var_offset = Tree.MOVE (temp, Tree.MEM (Tree.BINOP (Tree.PLUS, frameTemp, Tree.CONST var_offset)))

    exception EmptySeqFrame

    fun seq [st] = st
        | seq (st :: st_list) = Tree.SEQ (st, seq st_list)
        | seq [] = raise EmptySeqFrame

    fun callFunction funcLabel currFrame listOffset =
        let
            val numParam = List.length listOffset + 2
            val Frame (currOffset, _) = currFrame

            val storeFp = moveTempToFrame currOffset frameTemp

            fun foldFun (prevMemOffset, (currOffset, currStmtList)) =
                let
                    val stmt = [
                        moveFrameToTemp argTemp prevMemOffset,
                        moveTempToFrame currOffset argTemp
                    ]
                in
                    (currOffset + wordSize, currStmtList @ stmt)
                end

            val (_, storeArgStmtList) =  
                foldl foldFun (currOffset + 2 * wordSize, []) listOffset
            val storeArgStmt = seq storeArgStmtList 

            val updateFpSp = seq [
                Tree.MOVE (frameTemp, stackTemp),
                Tree.MOVE (stackTemp, Tree.BINOP (Tree.PLUS, stackTemp, Tree.CONST numParam))
            ]

            val callFunc = Tree.EXP (Tree.CALL (Tree.NAME funcLabel, []))

            val restoreFpSp = seq [
                Tree.MOVE (stackTemp, frameTemp),
                moveFrameToTemp frameTemp 0
            ]

            val moveRetValToResult = Tree.MOVE (resultTemp, returnTemp)

        in
            seq [
                storeFp,            (* Store frame pointer *)
                storeArgStmt,       (* Store arguments *)
                updateFpSp,         (* Update FP, SP *)
                callFunc,           (* Call function *)
                restoreFpSp,        (* Restore FP, SP *)
                moveRetValToResult  (* Get return value in result *)
            ]
        end
end