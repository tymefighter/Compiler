type lexresult = Tokens.token

fun eof () = Tokens.EOF

(* Square brackets are not added yet *)

%%
%structure TigerLex

alpha = [a-zA-Z];
number = [0-9]+;
sym = [,:(){}+-/=<>&|];
whitespace = [\ \t];

%%
{whitespace}+   => (lex() (* whitespace *));
\n              => (lex() (* newline *));
"/*".*"*/"       => (lex() (* comment *));
number          => (Tokens.Number yytext);
sym             => (Tokens.Symbols yytext);
{alpha}+        => (Tokens.AlphaStr yytext);