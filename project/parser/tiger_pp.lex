type pos = int

type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNo = ref 0
val posInLine = ref 0

exception SyntaxError

fun inc ref_x = ref_x := !ref_x + 1
fun inc_n ref_x n = ref_x := !ref_x + n
fun reset ref_x = ref_x := 0

fun eof () = Tokens.EOF (!lineNo, !posInLine)

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));

alpha = [a-zA-Z];
digit = [0-9];
space = [\ ];
tabspace = [\t];

%%

\n => (let 
        val _ = inc lineNo
        val _ = reset posInLine
    in
        lex()
    end);

{space}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        lex()
    end);

{tabspace}+ => (let
        val _ = inc_n posInLine (8 * (size yytext))
    in
        lex()
    end);

{digit}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.INT (yytext, !lineNo, !posInLine)
    end);

"\""([^"])*s"\"" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.STR (yytext, !lineNo, !posInLine)
    end);

"nil" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NIL (!lineNo, !posInLine)
    end);

"if" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IF (!lineNo, !posInLine)
    end);

"then" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.THEN (!lineNo, !posInLine)
    end);

"else" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ELSE (!lineNo, !posInLine)
    end);

"while" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.WHILE (!lineNo, !posInLine)
    end);

"do" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DO (!lineNo, !posInLine)
    end);

"for" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.FOR (!lineNo, !posInLine)
    end);

":=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ASSIGN (!lineNo, !posInLine)
    end);

"to" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.TO (!lineNo, !posInLine)
    end);

"break" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.BREAK (!lineNo, !posInLine)
    end);

"of" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.OF (!lineNo, !posInLine)
    end);

"type" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.TYPE (!lineNo, !posInLine)
    end);

"var" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.VAR (!lineNo, !posInLine)
    end);

"let" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LET (!lineNo, !posInLine)
    end);

"in" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IN (!lineNo, !posInLine)
    end);

"end" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.END (!lineNo, !posInLine)
    end);

"array" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ARRAY (!lineNo, !posInLine)
    end);

"import" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IMPORT (!lineNo, !posInLine)
    end);

{alpha}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ID (yytext, !lineNo, !posInLine)
    end);

":" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.COLON (!lineNo, !posInLine)
    end);

"+" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ADD (!lineNo, !posInLine)
    end);

"-" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SUB (!lineNo, !posInLine)
    end);

"*" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.MUL (!lineNo, !posInLine)
    end);

"/" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DIV (!lineNo, !posInLine)
    end);

"=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.EQ (!lineNo, !posInLine)
    end);

"<>" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NE (!lineNo, !posInLine)
    end);

">=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.GE (!lineNo, !posInLine)
    end);

"<=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LE (!lineNo, !posInLine)
    end);

">" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.G (!lineNo, !posInLine)
    end);
    

"<" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.L (!lineNo, !posInLine)
    end);

"&" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.AND (!lineNo, !posInLine)
    end);


"|" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.OR (!lineNo, !posInLine)
    end);

"(" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_B (!lineNo, !posInLine)
    end);

")" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_B (!lineNo, !posInLine)
    end);

";" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SEMI (!lineNo, !posInLine)
    end);

"," => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.COMMA (!lineNo, !posInLine)
    end);

"." => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DOT (!lineNo, !posInLine)
    end);

"[" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_SQ (!lineNo, !posInLine)
    end);

"]" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_SQ (!lineNo, !posInLine)
    end);

. => (let
    val _ = print ("Syntax error on line " ^ (Int.toString (!lineNo)) ^ " and " ^ (Int.toString (!posInLine)) ^ " char\n")
    in
        raise SyntaxError
    end);