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

    fun pprintTyfields [] = ""
        | pprintTyfields [(id_left, id_right)] = (ind ()) ^ id_left ^ " : " ^ id_right ^ "\n"
        | pprintTyfields ((id_left, id_right) :: xs) = (ind ()) ^ id_left ^ " : " ^ id_right ^ ",\n" ^ pprintTyfields xs
    
    fun pprintType (Ast.Alias tp) = tp
        | pprintType (Ast.Array arr) = "array of " ^ arr
        | pprintType (Ast.RecordType tyfds) = let
                val str1 = "{\n"
                val _ = inc indentation_level
                val str2 = pprintTyfields tyfds
                val _ = dec indentation_level
                val str3 =  (ind ()) ^ "}"
            in
                str1 ^ str2 ^ str3
            end

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
        | pprintExp (Ast.Lval lval) = pprintLval lval
        | pprintExp (Ast.FunCall (func_name, ls)) = func_name ^ " (" ^ pprintParamList ls ^ ")"
        | pprintExp (Ast.MethodCall (lval, m_name, ls)) = pprintLval lval ^ "." ^ m_name ^ " (" ^ pprintParamList ls ^ ")"
        | pprintExp (Ast.LetStmt (d_list, e_list)) = let
                val str1 = "let\n"
                val _ = inc indentation_level
                val str2 = pprintDecList d_list
                val _ = dec indentation_level
                val str3 = (ind ()) ^ "in\n"
                val _ = inc indentation_level
                val str4 = pprintExpList e_list
                val _ = dec indentation_level
                val str5 = "\n" ^ (ind ()) ^ "end"
            in
                str1 ^ str2 ^ str3 ^ str4 ^ str5
            end

    and  pprintExpList [] = ""
    | pprintExpList [exp] = (ind ()) ^ "(" ^ (pprintExp exp) ^ ")"
    | pprintExpList (exp_list) = let
        val str_begin = (ind ()) ^ "(\n"
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

    and pprintDec (Ast.Vardec (id, type_id_opt, exp)) = let
            val str1 = "var " ^ id
            val str2 = case type_id_opt of 
                SOME (type_id) => " : " ^ type_id
                | NONE => ""
            val str3 = " := " ^ pprintExp exp
        in
            str1 ^ str2 ^ str3
        end
    | pprintDec (Ast.Typedec (id, tp)) = "type " ^ id ^ " = " ^ pprintType tp
    | pprintDec (Ast.Import id) = "import " ^ id
    | pprintDec (Ast.FuncDec (id, tyfds, tp_opt, exp)) = let
            val str1 = "function " ^ id ^ " "
            val str2 = "(\n"
            val _ = inc indentation_level
            val str3 = pprintTyfields tyfds
            val _ = dec indentation_level
            val str4 =  (ind ()) ^ ")"
            val str5 = case tp_opt of 
                SOME (tp) => " : " ^ tp
                | NONE => ""
            val str6 = " = " ^ pprintExp exp
        in
            str1 ^ str2 ^ str3 ^ str4 ^ str5 ^ str6
        end

    and pprintDecList [] = ""
        | pprintDecList (d :: d_list) = (ind()) ^ pprintDec d ^ "\n" ^ pprintDecList d_list

    fun pprintProg (Ast.Expression exp) = (pprintExp exp) ^ "\n"
        | pprintProg (Ast.Decs d_list) = pprintDecList d_list

end

val _ = print (Pprint.pprintProg prog)