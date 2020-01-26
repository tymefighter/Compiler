type pos = int

type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

fun eof () => Tokens.EOF

val lineNo = ref 0
val posInLine = ref 0

exception SyntaxError

fun inc ref_x = ref_x := !ref_x + 1
fun inc_n ref_x n = ref_x := !ref_x + n
fun reset ref_x = ref_x := 0


%%

%header (functor TigerLexFun(structure Tokens));

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

{alpha}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ID (yytext, !lineNo, !posInLine)
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

. => (let
    val _ = print "Syntax error on line " ^ (Int.toString !lineNo) ^ " and " ^ (Int.toString !posInLine) ^ " char"
    in
        raise SyntaxError
    end);