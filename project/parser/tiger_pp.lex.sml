(*#line 20.10 "tiger_pp.lex"*)functor TigerLexFun(structure Tokens : Tiger_TOKENS)(*#line 1.1 "tiger_pp.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "tiger_pp.lex"*)type pos = int

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

(*#line 23.1 "tiger_pp.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\033\032\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\030\003\026\003\003\003\025\003\024\023\022\021\003\020\003\019\
\\017\017\017\017\017\017\017\017\017\017\003\016\013\012\010\003\
\\003\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\003\003\003\003\003\
\\003\005\005\005\005\005\005\005\005\005\005\005\005\005\007\005\
\\005\005\005\005\005\005\005\005\005\005\005\003\004\003\003\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\008\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (8, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\009\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\015\014\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\018\018\018\018\018\018\018\018\018\018\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (26, 
"\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\000\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\028\027\027\027\027\027\027\027\027\027\027\027\027\
\\027"
),
 (28, 
"\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\029\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\028\027\027\027\027\027\027\027\027\027\027\027\027\
\\027"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 57)], trans = 0},
{fin = [(N 49),(N 57)], trans = 0},
{fin = [(N 22),(N 57)], trans = 5},
{fin = [(N 22)], trans = 5},
{fin = [(N 22),(N 57)], trans = 7},
{fin = [(N 22)], trans = 8},
{fin = [(N 19),(N 22)], trans = 5},
{fin = [(N 43),(N 57)], trans = 10},
{fin = [(N 38)], trans = 0},
{fin = [(N 32),(N 57)], trans = 0},
{fin = [(N 45),(N 57)], trans = 13},
{fin = [(N 35)], trans = 0},
{fin = [(N 41)], trans = 0},
{fin = [(N 55),(N 57)], trans = 0},
{fin = [(N 10),(N 57)], trans = 17},
{fin = [(N 10)], trans = 17},
{fin = [(N 30),(N 57)], trans = 0},
{fin = [(N 26),(N 57)], trans = 0},
{fin = [(N 24),(N 57)], trans = 0},
{fin = [(N 28),(N 57)], trans = 0},
{fin = [(N 53),(N 57)], trans = 0},
{fin = [(N 51),(N 57)], trans = 0},
{fin = [(N 47),(N 57)], trans = 0},
{fin = [(N 57)], trans = 26},
{fin = [], trans = 26},
{fin = [], trans = 28},
{fin = [(N 15)], trans = 0},
{fin = [(N 4),(N 57)], trans = 30},
{fin = [(N 4)], trans = 30},
{fin = [(N 1)], trans = 0},
{fin = [(N 7),(N 57)], trans = 33},
{fin = [(N 7)], trans = 33}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 29.8 "tiger_pp.lex"*)let 
        val _ = inc lineNo
        val _ = reset posInLine
    in
        lex()
    end(*#line 256.1 "tiger_pp.lex.sml"*)
)
| 10 => let val yytext=yymktext() in (*#line 48.14 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.INT (yytext, !lineNo, !posInLine)
    end(*#line 262.1 "tiger_pp.lex.sml"*)
 end
| 15 => let val yytext=yymktext() in (*#line 54.22 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.STR (yytext, !lineNo, !posInLine)
    end(*#line 268.1 "tiger_pp.lex.sml"*)
 end
| 19 => let val yytext=yymktext() in (*#line 60.11 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NIL (!lineNo, !posInLine)
    end(*#line 274.1 "tiger_pp.lex.sml"*)
 end
| 22 => let val yytext=yymktext() in (*#line 66.14 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ID (yytext, !lineNo, !posInLine)
    end(*#line 280.1 "tiger_pp.lex.sml"*)
 end
| 24 => let val yytext=yymktext() in (*#line 72.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.ADD (!lineNo, !posInLine)
    end(*#line 286.1 "tiger_pp.lex.sml"*)
 end
| 26 => let val yytext=yymktext() in (*#line 78.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SUB (!lineNo, !posInLine)
    end(*#line 292.1 "tiger_pp.lex.sml"*)
 end
| 28 => let val yytext=yymktext() in (*#line 84.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.MUL (!lineNo, !posInLine)
    end(*#line 298.1 "tiger_pp.lex.sml"*)
 end
| 30 => let val yytext=yymktext() in (*#line 90.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.DIV (!lineNo, !posInLine)
    end(*#line 304.1 "tiger_pp.lex.sml"*)
 end
| 32 => let val yytext=yymktext() in (*#line 96.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.EQ (!lineNo, !posInLine)
    end(*#line 310.1 "tiger_pp.lex.sml"*)
 end
| 35 => let val yytext=yymktext() in (*#line 102.10 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.NE (!lineNo, !posInLine)
    end(*#line 316.1 "tiger_pp.lex.sml"*)
 end
| 38 => let val yytext=yymktext() in (*#line 108.10 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.GE (!lineNo, !posInLine)
    end(*#line 322.1 "tiger_pp.lex.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 36.14 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        lex()
    end(*#line 328.1 "tiger_pp.lex.sml"*)
 end
| 41 => let val yytext=yymktext() in (*#line 114.10 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LE (!lineNo, !posInLine)
    end(*#line 334.1 "tiger_pp.lex.sml"*)
 end
| 43 => let val yytext=yymktext() in (*#line 120.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.G (!lineNo, !posInLine)
    end(*#line 340.1 "tiger_pp.lex.sml"*)
 end
| 45 => let val yytext=yymktext() in (*#line 127.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.L (!lineNo, !posInLine)
    end(*#line 346.1 "tiger_pp.lex.sml"*)
 end
| 47 => let val yytext=yymktext() in (*#line 133.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.AND (!lineNo, !posInLine)
    end(*#line 352.1 "tiger_pp.lex.sml"*)
 end
| 49 => let val yytext=yymktext() in (*#line 140.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.OR (!lineNo, !posInLine)
    end(*#line 358.1 "tiger_pp.lex.sml"*)
 end
| 51 => let val yytext=yymktext() in (*#line 146.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.LEFT_B (!lineNo, !posInLine)
    end(*#line 364.1 "tiger_pp.lex.sml"*)
 end
| 53 => let val yytext=yymktext() in (*#line 152.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.RIGHT_B (!lineNo, !posInLine)
    end(*#line 370.1 "tiger_pp.lex.sml"*)
 end
| 55 => let val yytext=yymktext() in (*#line 158.9 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (size yytext)
    in
        Tokens.SEMI (!lineNo, !posInLine)
    end(*#line 376.1 "tiger_pp.lex.sml"*)
 end
| 57 => ((*#line 164.7 "tiger_pp.lex"*)let
    val _ = print ("Syntax error on line " ^ (Int.toString (!lineNo)) ^ " and " ^ (Int.toString (!posInLine)) ^ " char\n")
    in
        raise SyntaxError
    end(*#line 382.1 "tiger_pp.lex.sml"*)
)
| 7 => let val yytext=yymktext() in (*#line 42.17 "tiger_pp.lex"*)let
        val _ = inc_n posInLine (8 * (size yytext))
    in
        lex()
    end(*#line 388.1 "tiger_pp.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
