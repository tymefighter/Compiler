functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "tiger_pp.grm"*)

(*#line 13.1 "tiger_pp.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\002\000\007\000\004\000\006\000\006\000\005\000\
\\017\000\004\000\000\000\
\\001\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\
\\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\015\000\010\000\016\000\009\000\
\\019\000\037\000\000\000\
\\001\000\018\000\036\000\000\000\
\\001\000\020\000\000\000\000\000\
\\040\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\
\\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\015\000\010\000\016\000\009\000\000\000\
\\041\000\001\000\008\000\002\000\007\000\004\000\006\000\006\000\005\000\
\\017\000\004\000\000\000\
\\042\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\007\000\018\000\008\000\017\000\000\000\
\\047\000\007\000\018\000\008\000\017\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\051\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\052\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\053\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\054\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\055\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\000\000\
\\056\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\
\\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\000\000\
\\057\000\005\000\020\000\006\000\019\000\007\000\018\000\008\000\017\000\
\\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\014\000\011\000\015\000\010\000\000\000\
\\058\000\007\000\018\000\008\000\017\000\000\000\
\\059\000\000\000\
\"
val actionRowNumbers =
"\000\000\004\000\005\000\000\000\
\\007\000\009\000\008\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\002\000\
\\001\000\022\000\021\000\020\000\
\\019\000\018\000\017\000\016\000\
\\015\000\014\000\013\000\012\000\
\\011\000\010\000\023\000\005\000\
\\006\000\003\000"
val gotoT =
"\
\\001\000\001\000\002\000\037\000\000\000\
\\000\000\
\\001\000\020\000\003\000\019\000\000\000\
\\001\000\021\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\022\000\000\000\
\\001\000\023\000\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\001\000\026\000\000\000\
\\001\000\027\000\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\001\000\030\000\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\020\000\003\000\036\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 38
val numrules = 20
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | ID of  (string) | STR of  (string) | INT of  (string) | EXPS of  (Ast.Exp list) | PROG of  (Ast.Prog) | EXP of  (Ast.Exp)
end
type svalue = MlyValue.svalue
type result = Ast.Prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 19) => true | _ => false
val showTerminal =
fn (T 0) => "INT"
  | (T 1) => "STR"
  | (T 2) => "ID"
  | (T 3) => "NIL"
  | (T 4) => "ADD"
  | (T 5) => "SUB"
  | (T 6) => "MUL"
  | (T 7) => "DIV"
  | (T 8) => "EQ"
  | (T 9) => "NE"
  | (T 10) => "G"
  | (T 11) => "L"
  | (T 12) => "GE"
  | (T 13) => "LE"
  | (T 14) => "AND"
  | (T 15) => "OR"
  | (T 16) => "LEFT_B"
  | (T 17) => "RIGHT_B"
  | (T 18) => "SEMI"
  | (T 19) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROG ((*#line 46.13 "tiger_pp.grm"*)Ast.Expression EXP(*#line 223.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.EXPS ((*#line 48.9 "tiger_pp.grm"*)[](*#line 227.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 49.22 "tiger_pp.grm"*)EXP :: EXPS(*#line 231.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 51.12 "tiger_pp.grm"*)Ast.LiteralNil(*#line 235.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 52.12 "tiger_pp.grm"*)Ast.LiteralInt INT(*#line 239.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.STR STR, STR1left, STR1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 53.12 "tiger_pp.grm"*)Ast.LiteralStr STR(*#line 243.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, STR1left, STR1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 54.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.ADD, EXP2)(*#line 247.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 55.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.SUB, EXP2)(*#line 251.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 56.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.MUL, EXP2)(*#line 255.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 57.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.DIV, EXP2)(*#line 259.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 58.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.EQ, EXP2)(*#line 263.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 59.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.NE, EXP2)(*#line 267.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 60.18 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.G, EXP2)(*#line 271.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 61.18 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.L, EXP2)(*#line 275.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 62.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.GE, EXP2)(*#line 279.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 63.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.LE, EXP2)(*#line 283.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 64.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.AND, EXP2)(*#line 287.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 65.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.OR, EXP2)(*#line 291.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, SUB1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.16 "tiger_pp.grm"*)Ast.NegExp EXP(*#line 295.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, SUB1left, EXP1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RIGHT_B1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: ( _, ( _, LEFT_B1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.28 "tiger_pp.grm"*)Ast.Exprs EXPS(*#line 299.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LEFT_B1left, RIGHT_B1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.PROG x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.INT i,p1,p2))
fun STR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.STR i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.ID i,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun G (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun L (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun LEFT_B (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun RIGHT_B (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
end
end
