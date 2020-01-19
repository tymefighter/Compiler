type lexresult = Tokens.token

fun eof () = Tokens.EOF

(* Square brackets are not added yet *)

%%
%structure TigerLex

alpha = [a-zA-Z];
digit = [0-9];
whitespace = [\ \t];

%%
{whitespace}+   => (lex() (* whitespace *));
\n              => (lex() (* newline *));
"/*".*"*/"      => (lex() (* comment *));
{digit}+        => (Tokens.Number yytext);
"," | ";" | ":" | "(" | ")" | "{" | "}" | "+" | "-" | "/" | "=" | "<" | ">" | "&" | "|" | "[" | "]" => (Tokens.Symbol yytext);
{alpha}+        => (Tokens.AlphaStr yytext);