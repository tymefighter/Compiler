(* Internal datatypes and functions required by the lexer *)
(* Keeping track of position in source                    *)

type lineNo            = int
type pos               = lineNo  (* The type of Should match with expr.yacc *)
val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.
				    Typing not necessary just for clarity *)

fun updateLine n      = lineRef := !(lineRef) + n

(* Stuff done to make use of the Tokens module generated by expr.grm *)

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  (*else ("line " ^ Int.toString l ^ "-" ^ Int.toString r)*)
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")

(*
   What to return at the end of the file. Note the extra (!pos,!pos). If you have
   the clause

   %term FOO of int  | BAR

   The token module will have two functions which are

   Token.FOO : int * pos * pos
   Token.BAR : pos * pos

   Here we give the eof function for the lexer which should return the
   EOF terminal to the parser.

*)
fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

(* Some helper functions during lexing *)

fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

%%
%header (functor ExprLexFun(structure Tokens : Expr_TOKENS));
ws    = [\ \t];
digit = [0-9]+;
%%
"#".*\n       => ( updateLine 1; lex ());
{ws}+         => ( lex() );
\n({ws}*\n)*  => ( let val old = !lineRef
		   in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
		   end
		 );
{digit}+      => ( Tokens.CONST (toInt yytext, !lineRef, !lineRef) );
"+"           => ( Tokens.PLUS  (!lineRef,!lineRef) );
"-"           => ( Tokens.MINUS  (!lineRef,!lineRef) );
"*"           => ( Tokens.MUL (!lineRef,!lineRef) );
"/"           => ( Tokens.DIV (!lineRef,!lineRef) );
"("           => ( Tokens.LEFT_B (!lineRef,!lineRef));
")"           => ( Tokens.RIGHT_B (!lineRef,!lineRef));