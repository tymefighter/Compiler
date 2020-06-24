structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                            structure Lex = TigerLex
                            structure LrParser = LrParser)


val lexer = TigerParser.makeLexer (fn n => TextIO.inputN (TextIO.stdIn, n))

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (prog, _) = TigerParser.parse (0, lexer, print_error, ())

exception RestrictionFailedDriver

(* val (opt_func_dec, main_code) = case prog of
    Ast.Expression (Ast.LetStmt(dec_list, exp_list)) =>
        if(dec_list = []) then
            (NONE, Ast.Expression (Ast.Exprs exp_list))
        else
            (SOME (Ast.Decs dec_list), Ast.Expression(Ast.Exprs exp_list))
    | _ => raise RestrictionFailedDriver

val main_tree_code = Translate.translateProg main_code
(* val _ = print (Tree.pprintExp main_tree_code ^ "\n") *)
val main_mips_code = 
    MIPS.Label "main"
    :: MIPS.Instruction (MIPS.DataMove (MIPS.Move (MIPS.Fp, MIPS.Sp)))
    :: CodeGen.generateProg main_tree_code

val mips_code = 
    case opt_func_dec of
        NONE => main_mips_code
        | SOME (dec_tree_code) =>
            (MIPS.Instruction (MIPS.BranchJump (MIPS.Jump "main")))
            :: (
                CodeGen.generateProg (Translate.translateProg dec_tree_code)
                @ main_mips_code
            ) *)

val funcMap = Func.constructFuncMap prog
val treeCode = Translate.translateProg funcMap prog
val mips_code = CodeGen.generateProg treeCode
val _ = print (MIPS.printProg mips_code)