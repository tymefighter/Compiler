type lexresult = Tokens.token

fun eof () => Tokens.EOF

%%

%header (functor TigerLexFun(structure Tokens));

alpha = [a-zA-Z];
digit = [0-9];
whitespace = [\ \t];

%%

{whitespace}+ => (lex());

{digit}+ => (Tokens.INT yytext);

"\""([^"])*s"\"" =>(Tokens.STR yytext);

"nil" => (Tokens.NIL);

{alpha}+ => (Tokens.ID yytext);

"+" => (Tokens.ADD);

"-" => (Tokens.SUB);

"*" => (Tokens.MUL);

"/" => (Tokens.DIV);

"=" => (Tokens.EQ);

"<>" => (Tokens.NE);

">=" => (Tokens.GE);

"<=" => (Tokens.LE);

">" => (Tokens.G);

"<" => (Tokens.L);

"&" => (Tokens.AND);

"|" => (Tokens.OR);

"(" => (Tokens.LEFT_B);

")" => (Tokens.RIGHT_B);

";" => (Tokens.SEMI);