signature FRAME = sig
    type Offset
    type Frame
    val wordSize : Offset
    val stackAllocStmt : int -> Tree.stm
    val allocVar : Ast.id -> Frame -> Ast.id -> Frame
    val allocInternalVar : Frame -> Frame * int
    val getOffset : Frame -> Ast.id -> (Ast.id * int) option
    val emptyFrame : Frame

    val getFramePointer : Func.FuncMap -> Ast.id -> Ast.id -> Tree.stm
    val funcDecl : Frame -> Ast.id list -> Ast.id -> Frame
    val callFunction :  Func.FuncMap -> Ast.id -> Ast.id -> Temp.label -> Frame -> int list -> Tree.stm
    val getWordSize : int
end	

structure Frame :> FRAME = struct

    type Offset = int

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    datatype Frame = Frame of Offset * ((Ast.id * Offset) IdMap.map)
    (* The map in frame outputs name of function and offset
        given the name of a variable *)

    val wordSize = 4

    fun stackAllocStmt numAllocs = Tree.SEQ (
        Tree.MOVE (Tree.argTemp1, Tree.CONST (~wordSize * numAllocs)),
        Tree.MOVE (Tree.stackTemp, Tree.BINOP (Tree.PLUS, Tree.stackTemp, Tree.argTemp1))
    )

    fun allocVar currentFunc (Frame (currOffset, varMap)) var = 
        let
            val newMap = case IdMap.find (varMap, var) of
                NONE => IdMap.insert (varMap, var, (currentFunc, currOffset))
                | SOME prevOffset =>
                    let
                        val (mapAfterRemove, _) = IdMap.remove (varMap, var)
                    in
                        IdMap.insert (mapAfterRemove, var, (currentFunc, currOffset))
                    end

            val newOffset = currOffset - wordSize
        in
            Frame (newOffset, newMap)
        end

    fun allocInternalVar (Frame (currOffset, varMap)) = (Frame (currOffset - wordSize, varMap), currOffset)

    fun getOffset (Frame (_, varMap)) var = IdMap.find (varMap, var)

    val emptyFrame = Frame (0, IdMap.empty)

    exception NotInScope
    exception FunctionNotInScope

    fun searchRecursive funcMap prevStmList func searchFunc =
        (*
            Search ancestors of func to check if searchFunc is found,
            and if found place the frame pointer value of searchFunc
            into the resultTemp temporary at the end

            At the start of the function, resultTemp must contain
            the frame pointer value of func
        *)
        if(func = searchFunc) then
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
                searchRecursive funcMap newStmList parentOfFunc searchFunc
            end

    fun searchAncestor funcMap currFunc searchFunc =
        searchRecursive funcMap [Tree.MOVE (Tree.resultTemp, Tree.frameTemp)] currFunc searchFunc

    (* 
        Get ancFunc's frame pointer in resultTemp, where ancFunc is an 
        ancestor of currFunc (if not we raise error)
    *)
    fun getFramePointer funcMap currFunc ancFunc = 
        searchAncestor funcMap currFunc ancFunc

    fun getStaticLink funcMap caller callee = 
        let
            val calleeParent = case Func.find (funcMap, callee) of
                SOME func => func
                | NONE => raise FunctionNotInScope
        in
            (*
                Search ancestors of caller to check if parent of callee is found,
                at the start of each step in recursion, resultTemp contains frame
                pointer value of func (i.e. static link of any child of func),
                we stop when parent of callee matches func
            *)
            searchAncestor funcMap caller calleeParent
        end

    fun funcDecl prevFrame argNameList funcName =
        let
            fun placeVarInMap (argName, currFrame) = 
                let
                    val Frame (currOffset, currMap) = currFrame
                    val removeMap = case IdMap.find (currMap, argName) of
                        NONE => currMap
                        | SOME _ =>
                            let
                                val (remove_map, _) = IdMap.remove (currMap, argName)
                            in
                                remove_map
                            end
                in
                    Frame (currOffset - wordSize, IdMap.insert (removeMap, argName, (funcName, currOffset)))
                end
            val Frame (_, prevMap) = prevFrame
        in
            foldl placeVarInMap (Frame (~4 * wordSize, prevMap)) argNameList
        end

    fun callFunction funcMap caller callee funcLabel currFrame listOffset =
        let
            val numParam = List.length listOffset + 4
            val Frame (currOffset, _) = currFrame

            val storeFp = Tree.moveTempToFrame currOffset Tree.frameTemp

            val storeStaticLink = Tree.seq [
                getStaticLink funcMap caller callee,
                Tree.moveTempToFrame (currOffset - 2 * wordSize) Tree.resultTemp
            ]

            val storeSp = Tree.moveTempToFrame (currOffset - 3 * wordSize) Tree.frameTemp

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
                            foldl foldFun (currOffset - 4 * wordSize, []) list_offset
                    in
                        SOME (Tree.seq storeArgStmtList)
                    end

            val updateFpSp = Tree.seq [
                Tree.MOVE (Tree.argTemp1, Tree.CONST currOffset),
                Tree.MOVE (Tree.frameTemp, Tree.BINOP (Tree.PLUS, Tree.frameTemp, Tree.argTemp1)),
                Tree.MOVE (Tree.argTemp1, Tree.CONST (~numParam * wordSize)),
                Tree.MOVE (Tree.stackTemp, Tree.BINOP (Tree.PLUS, Tree.frameTemp, Tree.argTemp1))
            ]

            val callFunc = Tree.EXP (Tree.CALL (Tree.NAME funcLabel, []))

            val restoreFpSp = Tree.seq [
                Tree.moveFrameToTemp Tree.stackTemp (~3 * wordSize),
                Tree.moveFrameToTemp Tree.frameTemp 0
            ]

            val moveRetValToResult = Tree.MOVE (Tree.resultTemp, Tree.returnTemp)

        in
            case optStoreArgStmt of
                SOME storeArgStmt =>
                    Tree.seq [
                        storeFp,            (* Store frame pointer *)
                        storeStaticLink,    (* Store static link *)
                        storeSp,
                        storeArgStmt,       (* Store arguments *)
                        updateFpSp,         (* Update FP, SP *)
                        callFunc,           (* Call function *)
                        restoreFpSp,        (* Restore FP, SP *)
                        moveRetValToResult  (* Get return value in result *)
                    ]
                | NONE =>
                    Tree.seq [
                        storeFp,            (* Store frame pointer *)
                        storeStaticLink,    (* Store static link *)
                        storeSp,
                        updateFpSp,         (* Update FP, SP *)
                        callFunc,           (* Call function *)
                        restoreFpSp,        (* Restore FP, SP *)
                        moveRetValToResult  (* Get return value in result *)
                    ]
        end

        val getWordSize = wordSize
end