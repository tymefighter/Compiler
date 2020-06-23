signature FUNC = sig
    type FuncMap
    val emptyMap : FuncMap
    val constructFuncMap : Ast.Prog -> FuncMap
    val getStaticLink : (* Would raise FunctionNotInScope exception if callee is not in scope *)
    FuncMap (* Program function map *)
    ->  Ast.id (* Caller *)
    ->  Ast.id (* Callee *)
    ->  Tree.stm
    (* Statements to place static link into resultTemp *)
end

structure Func :> FUNC = struct

    exception RepeatedFuncNameUsage
    exception FunctionNotInScope

    structure IdMap = RedBlackMapFn (Ast.IdKey)
    type FuncMap = Ast.id IdMap.map

    val emptyMap = IdMap.empty

    fun foldFuncMapExpList parentFunc initFuncMap expList = 
        let
            fun foldFunc (exp, currFunMap) = constrFromExp parentFunc currFunMap exp
        in
            foldl foldFunc initFuncMap expList
        end

    and foldFuncMapDecList parentFunc initFuncMap decList = 
        let
            fun foldFunc (dec, currFunMap) = constrFromDec parentFunc currFunMap dec
        in
            foldl foldFunc initFuncMap decList
        end
    
    and constrFromExp parentFunc (funcMap : FuncMap) (exp : Ast.Exp) : FuncMap = case exp of
        Ast.Op (exp1, _, exp2) => foldFuncMapExpList parentFunc funcMap [exp1, exp2]
        | (Ast.NegExp exp) => constrFromExp parentFunc funcMap exp
        | (Ast.Exprs expList) => foldFuncMapExpList parentFunc funcMap expList
        | (Ast.IfThen (condExp, exp)) => foldFuncMapExpList parentFunc funcMap [condExp, exp]
        | (Ast.IfThenElse (condExp, trExp, flExp)) =>
            foldFuncMapExpList parentFunc funcMap [condExp, trExp, flExp]
        | (Ast.While (condExp, exp)) => foldFuncMapExpList parentFunc funcMap [condExp, exp]
        | (Ast.For (_, startExp, endExp, bodyExp)) =>
            foldFuncMapExpList parentFunc funcMap [startExp, endExp, bodyExp]
        | (Ast.Assignment (_, exp)) => constrFromExp parentFunc funcMap exp
        | (Ast.LetStmt (decList, expList)) =>
            let
                val funcMap1 = foldFuncMapDecList parentFunc funcMap decList
                val funcMap2 = foldFuncMapExpList parentFunc funcMap1 expList
            in
                funcMap2
            end
        | (Ast.FunCall (_, expList)) => foldFuncMapExpList parentFunc funcMap expList
        | _ => funcMap

    and constrFromDec parentFunc (funcMap : FuncMap) (dec : Ast.Dec) : FuncMap = case dec of
        (Ast.FuncDec (funcName, _, _, funcExp)) =>
            let
                val funcMap1 = case parentFunc of
                    NONE => funcMap
                    | SOME parentFuncName =>
                        IdMap.insert (funcMap, funcName, parentFuncName)
                val funcMap2 = constrFromExp (SOME funcName) funcMap1 funcExp
            in
                funcMap2
            end
        | _ => funcMap

    fun constructFuncMap (Ast.Expression exp) = constrFromExp NONE emptyMap exp
        | constructFuncMap (Ast.Decs decList) = foldFuncMapDecList NONE emptyMap decList

    (* fun getStaticLink (funcMap : FuncMap) (caller : Ast.id) (callee : Ast.id) =  *)


end