type lexresult = Tokens.token

fun eof () = Tokens.EOF

val pos_from_prev = ref 0
val line_no = ref 0

fun inc x = let
        val _ = x := !x + 1
    in
        ()
    end

fun assign x vl = let
        val _ = x := vl
    in
        ()
    end

fun reset x = let
        val _ = x := 0
    in
        ()
    end

(* Square brackets are not added yet *)

%%
%structure TigerLex

alpha = [a-zA-Z];
digit = [0-9];
whitespace = [\ \t];

%%
\              => (let
                        val _ = inc pos_from_prev
                    in
                        lex() (* whitespace *)
                    end);

\t              =>(let
                        val _ = assign pos_from_prev (!pos_from_prev + 8)
                    in
                        lex() (* whitespace *)
                    end);

\n              => (let
                        val _ = inc line_no
                        val _ = reset pos_from_prev
                    in 
                        lex() (* newline *)
                    end);

"/*"(\n | .)*"*/"      => (let
                        val p_prev = !pos_from_prev
                        val _ = reset pos_from_prev
                    in
                        Tokens.Comment (yytext, !line_no, p_prev)
                    end (* comment *));

{digit}+        => (let
                        val p_prev = !pos_from_prev
                        val _ = reset pos_from_prev
                    in
                        Tokens.Number (yytext, !line_no, p_prev)
                    end);

"." | "," | ";" | ":" | "(" | ")" | "{" | "}" | "+" | "-" | "/" | "=" | "<" | ">" | "&" | "|" | "[" | "]" => (let
                        val p_prev = !pos_from_prev
                        val _ = reset pos_from_prev
                    in
                        Tokens.Symbol (yytext, !line_no, p_prev)
                    end);
{alpha}+        => (let
                        val p_prev = !pos_from_prev
                        val _ = reset pos_from_prev
                    in
                        Tokens.AlphaStr (yytext, !line_no, p_prev)
                    end);