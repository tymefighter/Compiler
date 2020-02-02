structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure TigerLex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerParser = Join(structure ParserData = TigerLrVals.ParserData
                            structure Lex = TigerLex
                            structure LrParser = LrParser)


val lexer = TigerParser.makeLexer (fn n => TextIO.inputN (TextIO.stdIn, n))

fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (prog, _) = TigerParser.parse (0, lexer, print_error, ())


structure Pprint = struct

    val indentation_level = ref 0

    fun inc ref_x = ref_x := !ref_x + 1
    fun dec ref_x = ref_x := !ref_x - 1
    
    fun getSpaces s n = if(n <= 0) then s
        else getSpaces (" " ^ s) (n - 1)

    fun ind () = getSpaces "" (!indentation_level)

    fun pprintOp Ast.ADD = "+"
        | pprintOp Ast.SUB = "-"
        | pprintOp Ast.MUL = "*"
        | pprintOp Ast.DIV = "/"
        | pprintOp Ast.EQ = "="
        | pprintOp Ast.NE = "<>"
        | pprintOp Ast.G = ">"
        | pprintOp Ast.L = "<"
        | pprintOp Ast.GE = ">="
        | pprintOp Ast.LE = "<="
        | pprintOp Ast.AND = "&"
        | pprintOp Ast.OR = "|"

    fun pprintExp Ast.LiteralNil = "nil"
        | pprintExp (Ast.LiteralInt int_num_str) = int_num_str
        | pprintExp (Ast.LiteralStr str) = str

        | pprintExp (Ast.Op (exp1, oper, exp2)) = (pprintExp exp1) ^ " " ^ (pprintOp oper) ^ " " ^ (pprintExp exp2)
        | pprintExp (Ast.NegExp exp) = (pprintOp Ast.SUB) ^ " " ^ (pprintExp exp)
        | pprintExp (Ast.Exprs exp_list) = pprintExpList exp_list

        | pprintExp (Ast.IfThen (exp1, exp2)) = (
            let
                val str1 = "if " ^ (pprintExp exp1) ^ " then\n"
                val _ = inc indentation_level
                val str2 = (ind ()) ^ (pprintExp exp2)
                val _ = dec indentation_level
            in
                str1 ^ str2
            end
        )
        | pprintExp (Ast.IfThenElse (exp1, exp2, exp3)) = (
            let
                val str1 = "if " ^ (pprintExp exp1) ^ " then\n"
                val _ = inc indentation_level
                val str2 = (ind ()) ^ (pprintExp exp2) ^ "\n"
                val _ = dec indentation_level
                val str3 = (ind ()) ^ "else\n"
                val _ = inc indentation_level
                val str4 = (ind ()) ^ (pprintExp exp3)
                val _ = dec indentation_level
            in
                str1 ^ str2 ^ str3 ^ str4
            end
        )
        | pprintExp (Ast.While (exp1, exp2)) = (
            let
                val str1 = "while " ^ (pprintExp exp1) ^ " do\n"
                val _ = inc indentation_level
                val str2 = (ind ()) ^ (pprintExp exp2)
                val _ = dec indentation_level
            in
                str1 ^ str2
            end
        )
        | pprintExp (Ast.For (id, exp1, exp2, exp3)) = (
            let
                val str1 = "for " ^ id ^ " := " ^ (pprintExp exp1) ^ " to " ^ (pprintExp exp2) ^ " do\n"
                val _ = inc indentation_level
                val str2 = (ind ()) ^ (pprintExp exp3)
                val _ = dec indentation_level
            in
                str1 ^ str2
            end
        )
        | pprintExp (Ast.Break) = "break"
        | pprintExp (Ast.FunCall (func_name, ls)) = func_name ^ " (" ^ pprintParamList ls ^ ") "
        | pprintExp (Ast.Lval lval) = pprintLval lval

    and  pprintExpList [] = ""
    | pprintExpList [exp] = "(" ^ (pprintExp exp) ^ ")"
    | pprintExpList (exp_list) = let
        val str_begin = "(\n"
        val str_end = (ind ()) ^ ")"
        val _ = inc indentation_level
        val str = str_begin ^ pprintExpListHelper exp_list ^ str_end
        val _ = dec indentation_level
    in
        str
    end

    and pprintExpListHelper [] = ""
    | pprintExpListHelper (exp :: exp_list_tail) = (ind ()) ^ (pprintExp exp) ^ ";\n" ^ (pprintExpListHelper exp_list_tail)
    
    and pprintParamList [] = ""
    | pprintParamList [exp] = pprintExp exp
    | pprintParamList (exp :: exp_list) = pprintExp exp ^ ", " ^ pprintParamList exp_list

    and pprintLval (Ast.Var id) = id
    | pprintLval (Ast.MemberRef (lval, id)) = pprintLval lval ^ "." ^ id
    | pprintLval (Ast.IdxArr (lval, exp)) = pprintLval lval ^ "[" ^ pprintExp exp ^ "]"

    fun pprintProg (Ast.Expression exp) = (pprintExp exp) ^ "\n"

end

val _ = print (Pprint.pprintProg prog)