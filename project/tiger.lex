%%
%structure TigerLex

alpha = [a-zA-Z];
number = [0-9]+;
sym = [,:(){}\[\]+-/=<>&|];
whitespace = [\ \t];

%%
{whitespace}+   => (lex() (* whitespace *));
\n              => (lex() (* newline *));
"/*"*"*/"       => (lex() (* comment *));
number          => (SOME Tokens.Number yytext);
sym             => (SOME Tokens.Symbols yytext);
{alpha}+        => (SOME Tokens.AlphaStr yytext);
.               => (SOME Tokens.Error (*Anything else*));