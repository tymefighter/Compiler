signature FRAME = sig
    type Offset
    type Frame
    val wordSize : Offset
    val allocVar : Frame -> Ast.id -> Frame * Tree.stm
    val allocInternalVar : Frame -> Frame * int * Tree.stm
    val getOffset : Frame -> Ast.id -> int option
    val emptyFrame : Frame

    (* val newFuncFrame : Ast.id list -> Frame *)
end	

structure Frame :> FRAME = struct

    type Offset = int

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    datatype Frame = Frame of Offset * (Offset IdMap.map)

    val wordSize = 4

    val argTemp = Tree.TEMP Temp.argTemp1
    val stackTemp = Tree.TEMP Temp.stackPointer
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

    (* fun newFuncFrame varList =
        let
            fun helper (var :: varLs) frame = helper varLs (allocVar frame var)
                | helper _ frame = frame
        in
            helper varList emptyFrame
        end *)
end