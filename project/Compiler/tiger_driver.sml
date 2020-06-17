structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                            structure Lex = TigerLex
                            structure LrParser = LrParser)


val lexer = TigerParser.makeLexer (fn n => TextIO.inputN (TextIO.stdIn, n))

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (prog, _) = TigerParser.parse (0, lexer, print_error, ())

val tree_code = Translate.translateProg prog
val mips_code = CodeGen.generateEx tree_code
(* val _ = print (Tree.pprintExp tree_code ^ "\n") *)
val _ = print (MIPS.printProg mips_code)