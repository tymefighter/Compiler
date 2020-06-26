structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                            structure Lex = TigerLex
                            structure LrParser = LrParser)


val lexer = TigerParser.makeLexer (fn n => TextIO.inputN (TextIO.stdIn, n))

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (prog, _) = TigerParser.parse (0, lexer, print_error, ())

exception RestrictionFailedDriver

val funcMap = Func.constructFuncMap prog
val treeCode = Translate.translateProg funcMap prog
val mips_code = CodeGen.generateProg treeCode
val _ = print (MIPS.printProg mips_code)