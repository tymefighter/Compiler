signature FRAME = sig
    type Offset
    type frame
    val wordSize : Offset
    val allocVar : frame -> Ast.id -> frame
    val getOffset : frame -> Ast.id -> int option
    val emptyFrame : frame

    val newFuncFrame : Ast.id list -> frame
end	

structure Frame :> FRAME = struct

    type Offset = int

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    datatype Frame = Frame of Offset * (Offset IdMap.map)

    val wordSize = 4

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
            Frame (newOffset, newMap)
        end

    fun getOffset (Frame (_, varMap)) var = IdMap.find (varMap, var)

    val emptyFrame = (0, IdMap.empty)

    fun newFuncFrame varList =
        let
            fun helper (var :: varLs) frame = helper varLs (allocVar frame)
                | helper _ frame = frame
        in
            helper varList emptyFrame
        end
end