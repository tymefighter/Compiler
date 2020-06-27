structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                            structure Lex = TigerLex
                            structure LrParser = LrParser)

val (inputFile, optOutputFile, optIrFile) = Arg.getArg ()
val inputHandle = TextIO.openIn inputFile
val outputHandle = case optOutputFile of
    SOME outFile => TextIO.openOut outFile
    | NONE =>
        let
            val lenInp = String.size inputFile
            val lenExt = String.size Arg.extension
        in
            TextIO.openOut (String.substring (inputFile, 0, lenInp - lenExt) ^ ".s")
        end
val optIrHandle = case optIrFile of
    SOME irFile => SOME (TextIO.openOut irFile)
    | NONE => NONE

val lexer = TigerParser.makeLexer (fn n => TextIO.inputN (inputHandle, n))

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (prog, _) = TigerParser.parse (0, lexer, print_error, ())

val funcMap = Func.constructFuncMap prog
val treeCode = Translate.translateProg funcMap prog
val mipsCode = CodeGen.generateProg treeCode

val _ = TextIO.output (outputHandle, MIPS.printProg mipsCode)
val _ = case optIrHandle of
    SOME irHandle =>
        let
            val _ = TextIO.output (irHandle, Tree.pprintStm treeCode)
            val _ = TextIO.closeOut irHandle
        in
            ()
        end
    | NONE => ()
val _ = TextIO.closeIn inputHandle
val _ = TextIO.closeOut outputHandle