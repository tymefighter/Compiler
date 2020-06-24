type pos = int

type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNo = ref 0
val posInLine = ref 0
val nestingLevel = ref 0

exception SyntaxError

fun inc ref_x = ref_x := !ref_x + 1
fun dec ref_x = ref_x := !ref_x - 1
fun inc_n ref_x n = ref_x := !ref_x + n
fun reset ref_x = ref_x := 0

fun eof () = Tokens.EOF (!lineNo, !posInLine)

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
%s COMMENT;

alpha = [a-zA-Z];
digit = [0-9];
varallowed = [a-zA-Z0-9_];
space = [\ ];
tabspace = [\t];

%%

<INITIAL> "/*" => (
    inc nestingLevel;
    inc_n posInLine 2;
    YYBEGIN COMMENT;
    lex()
);

<COMMENT> "/*" => (
    inc nestingLevel;
    inc_n posInLine 2;
    YYBEGIN COMMENT;
    lex()
);

<COMMENT> "*/" => (
    dec nestingLevel;
    if(!nestingLevel = 0) then
    (
        YYBEGIN INITIAL;
        lex()
    )
    else
        lex()
);

<COMMENT> . => (let
        val _ = inc posInLine
    in
        lex()
    end);

<COMMENT> \n => (let
        val _ = reset posInLine
        val _ = inc lineNo
    in
        lex()
    end);

<INITIAL> \n => (let 
        val _ = inc lineNo
        val _ = reset posInLine
    in
        lex()
    end);

<INITIAL> {space}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        lex()
    end);

<INITIAL> {tabspace}+ => (let
        val _ = inc_n posInLine (8 * (size yytext))
    in
        lex()
    end);

<INITIAL> {digit}+ => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.INT (yytext, !lineNo, !posInLine)
    end);

<INITIAL> "\""([\\]. | [^"])*"\"" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.STR (yytext, !lineNo, !posInLine)
    end);

<INITIAL> "nil" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NIL (!lineNo, !posInLine)
    end);

<INITIAL> "if" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IF (!lineNo, !posInLine)
    end);

<INITIAL> "then" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.THEN (!lineNo, !posInLine)
    end);

<INITIAL> "else" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ELSE (!lineNo, !posInLine)
    end);

<INITIAL> "while" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.WHILE (!lineNo, !posInLine)
    end);

<INITIAL> "do" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DO (!lineNo, !posInLine)
    end);

<INITIAL> "for" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.FOR (!lineNo, !posInLine)
    end);

<INITIAL> ":=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ASSIGN (!lineNo, !posInLine)
    end);

<INITIAL> "to" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.TO (!lineNo, !posInLine)
    end);

<INITIAL> "break" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.BREAK (!lineNo, !posInLine)
    end);

<INITIAL> "of" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.OF (!lineNo, !posInLine)
    end);

<INITIAL> "type" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.TYPE (!lineNo, !posInLine)
    end);

<INITIAL> "var" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.VAR (!lineNo, !posInLine)
    end);

<INITIAL> "let" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LET (!lineNo, !posInLine)
    end);

<INITIAL> "in" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IN (!lineNo, !posInLine)
    end);

<INITIAL> "end" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.END (!lineNo, !posInLine)
    end);

<INITIAL> "array" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ARRAY (!lineNo, !posInLine)
    end);

<INITIAL> "import" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.IMPORT (!lineNo, !posInLine)
    end);

<INITIAL> "function" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.FUNCTION (!lineNo, !posInLine)
    end);

<INITIAL> "class" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.CLASS (!lineNo, !posInLine)
    end);

<INITIAL> "method" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.METHOD (!lineNo, !posInLine)
    end);

<INITIAL> "extends" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.EXTENDS (!lineNo, !posInLine)
    end);

<INITIAL> "primitve" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.PRIMITIVE (!lineNo, !posInLine)
    end);

<INITIAL> "new" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NEW (!lineNo, !posInLine)
    end);

<INITIAL> {alpha}{varallowed}* => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ID (yytext, !lineNo, !posInLine)
    end);

<INITIAL> ":" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.COLON (!lineNo, !posInLine)
    end);

<INITIAL> "+" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ADD (!lineNo, !posInLine)
    end);

<INITIAL> "-" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SUB (!lineNo, !posInLine)
    end);

<INITIAL> "*" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.MUL (!lineNo, !posInLine)
    end);

<INITIAL> "/" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DIV (!lineNo, !posInLine)
    end);

<INITIAL> "<<" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LSHIFT (!lineNo, !posInLine)
    end);

<INITIAL> ">>" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RSHIFT (!lineNo, !posInLine)
    end);

<INITIAL> "=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.EQ (!lineNo, !posInLine)
    end);

<INITIAL> "<>" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NE (!lineNo, !posInLine)
    end);

<INITIAL> ">=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.GE (!lineNo, !posInLine)
    end);

<INITIAL> "<=" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LE (!lineNo, !posInLine)
    end);

<INITIAL> ">" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.G (!lineNo, !posInLine)
    end);
    

<INITIAL> "<" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.L (!lineNo, !posInLine)
    end);

<INITIAL> "&" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.AND (!lineNo, !posInLine)
    end);


<INITIAL> "|" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.OR (!lineNo, !posInLine)
    end);

<INITIAL> "(" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_B (!lineNo, !posInLine)
    end);

<INITIAL> ")" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_B (!lineNo, !posInLine)
    end);

<INITIAL> ";" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SEMI (!lineNo, !posInLine)
    end);

<INITIAL> "," => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.COMMA (!lineNo, !posInLine)
    end);

<INITIAL> "." => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DOT (!lineNo, !posInLine)
    end);

<INITIAL> "[" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_SQ (!lineNo, !posInLine)
    end);

<INITIAL> "]" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_SQ (!lineNo, !posInLine)
    end);

<INITIAL> "{" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_CUR (!lineNo, !posInLine)
    end);

<INITIAL> "}" => (let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_CUR (!lineNo, !posInLine)
    end);

<INITIAL> . => (let
    val _ = print ("Syntax error on line " ^ (Int.toString (!lineNo)) ^ " and " ^ (Int.toString (!posInLine)) ^ " char\n")
    in
        raise SyntaxError
    end);