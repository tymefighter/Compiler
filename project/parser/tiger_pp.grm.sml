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
\\001\000\001\000\014\000\002\000\013\000\003\000\012\000\004\000\011\000\
\\006\000\010\000\017\000\009\000\021\000\008\000\024\000\007\000\
\\026\000\006\000\029\000\005\000\000\000\
\\001\000\003\000\029\000\000\000\
\\001\000\003\000\037\000\000\000\
\\001\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\022\000\052\000\000\000\
\\001\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\025\000\051\000\000\000\
\\001\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\025\000\072\000\000\000\
\\001\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\028\000\066\000\000\000\
\\001\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\033\000\057\000\000\000\
\\001\000\018\000\053\000\000\000\
\\001\000\018\000\063\000\000\000\
\\001\000\018\000\069\000\000\000\
\\001\000\020\000\000\000\000\000\
\\001\000\027\000\050\000\000\000\
\\075\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\000\000\
\\076\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\019\000\054\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\007\000\026\000\008\000\025\000\000\000\
\\082\000\007\000\026\000\008\000\025\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\086\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\087\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\088\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\089\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\090\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\091\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\000\000\
\\092\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\000\000\
\\093\000\007\000\026\000\008\000\025\000\000\000\
\\094\000\000\000\
\\095\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\023\000\067\000\000\000\
\\096\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\000\000\
\\097\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\000\000\
\\098\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\000\000\
\\099\000\000\000\
\\100\000\031\000\016\000\032\000\015\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\017\000\035\000\000\000\
\\104\000\017\000\058\000\000\000\
\\105\000\000\000\
\\106\000\001\000\014\000\002\000\013\000\003\000\012\000\004\000\011\000\
\\006\000\010\000\017\000\009\000\021\000\008\000\024\000\007\000\
\\026\000\006\000\029\000\005\000\000\000\
\\107\000\005\000\028\000\006\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\030\000\064\000\000\000\
\\108\000\000\000\
\"
val actionRowNumbers =
"\000\000\038\000\013\000\037\000\
\\001\000\000\000\000\000\000\000\
\\000\000\016\000\041\000\018\000\
\\017\000\000\000\002\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\012\000\
\\004\000\003\000\008\000\014\000\
\\031\000\044\000\007\000\042\000\
\\030\000\029\000\028\000\027\000\
\\026\000\025\000\024\000\023\000\
\\022\000\021\000\020\000\019\000\
\\000\000\000\000\000\000\032\000\
\\000\000\009\000\045\000\043\000\
\\044\000\006\000\035\000\033\000\
\\015\000\039\000\044\000\010\000\
\\000\000\000\000\046\000\040\000\
\\005\000\034\000\000\000\036\000\
\\011\000"
val gotoT =
"\
\\001\000\002\000\002\000\072\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\028\000\005\000\001\000\000\000\
\\001\000\029\000\005\000\001\000\000\000\
\\001\000\031\000\003\000\030\000\005\000\001\000\000\000\
\\001\000\032\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\034\000\005\000\001\000\000\000\
\\000\000\
\\001\000\036\000\005\000\001\000\000\000\
\\001\000\037\000\005\000\001\000\000\000\
\\001\000\038\000\005\000\001\000\000\000\
\\001\000\039\000\005\000\001\000\000\000\
\\001\000\040\000\005\000\001\000\000\000\
\\001\000\041\000\005\000\001\000\000\000\
\\001\000\042\000\005\000\001\000\000\000\
\\001\000\043\000\005\000\001\000\000\000\
\\001\000\044\000\005\000\001\000\000\000\
\\001\000\045\000\005\000\001\000\000\000\
\\001\000\046\000\005\000\001\000\000\000\
\\001\000\047\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\004\000\053\000\005\000\001\000\000\000\
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
\\001\000\057\000\005\000\001\000\000\000\
\\001\000\058\000\005\000\001\000\000\000\
\\001\000\059\000\005\000\001\000\000\000\
\\000\000\
\\001\000\031\000\003\000\060\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\004\000\063\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\054\000\004\000\066\000\005\000\001\000\000\000\
\\000\000\
\\001\000\068\000\005\000\001\000\000\000\
\\001\000\069\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\071\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 73
val numrules = 34
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
datatype svalue = VOID | ntVOID of unit | ID of  (string) | STR of  (string) | INT of  (string) | LVALUE of  (Ast.Lvalue) | PARAM of  (Ast.Exp list) | EXPS of  (Ast.Exp list) | PROG of  (Ast.Prog) | EXP of  (Ast.Exp)
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
  | (T 20) => "IF"
  | (T 21) => "THEN"
  | (T 22) => "ELSE"
  | (T 23) => "WHILE"
  | (T 24) => "DO"
  | (T 25) => "FOR"
  | (T 26) => "ASSIGN"
  | (T 27) => "TO"
  | (T 28) => "BREAK"
  | (T 29) => "COMMA"
  | (T 30) => "DOT"
  | (T 31) => "LEFT_SQ"
  | (T 32) => "RIGHT_SQ"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PROG ((*#line 61.13 "tiger_pp.grm"*)Ast.Expression EXP(*#line 332.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 63.13 "tiger_pp.grm"*)[EXP](*#line 336.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.EXPS EXPS, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS ((*#line 64.22 "tiger_pp.grm"*)EXP :: EXPS(*#line 340.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 3, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 66.12 "tiger_pp.grm"*)Ast.LiteralNil(*#line 344.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 67.12 "tiger_pp.grm"*)Ast.LiteralInt INT(*#line 348.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.STR STR, STR1left, STR1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 68.12 "tiger_pp.grm"*)Ast.LiteralStr STR(*#line 352.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, STR1left, STR1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 70.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.ADD, EXP2)(*#line 356.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 71.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.SUB, EXP2)(*#line 360.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 72.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.MUL, EXP2)(*#line 364.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 73.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.DIV, EXP2)(*#line 368.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 74.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.EQ, EXP2)(*#line 372.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 75.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.NE, EXP2)(*#line 376.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 76.18 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.G, EXP2)(*#line 380.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 77.18 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.L, EXP2)(*#line 384.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 78.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.GE, EXP2)(*#line 388.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 79.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.LE, EXP2)(*#line 392.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 80.20 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.AND, EXP2)(*#line 396.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 81.19 "tiger_pp.grm"*)Ast.Op (EXP1, Ast.OR, EXP2)(*#line 400.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.EXP EXP, _, EXP1right)) :: ( _, ( _, SUB1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 82.16 "tiger_pp.grm"*)Ast.NegExp EXP(*#line 404.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, SUB1left, EXP1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RIGHT_B1right)) :: ( _, ( MlyValue.EXPS EXPS, _, _)) :: ( _, ( _, LEFT_B1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 83.28 "tiger_pp.grm"*)Ast.Exprs EXPS(*#line 408.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LEFT_B1left, RIGHT_B1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 85.24 "tiger_pp.grm"*)Ast.IfThen (EXP1, EXP2)(*#line 412.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 86.33 "tiger_pp.grm"*)Ast.IfThenElse (EXP1, EXP2, EXP3)(*#line 416.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, IF1left, EXP3right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 88.25 "tiger_pp.grm"*)Ast.While (EXP1, EXP2)(*#line 420.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, WHILE1left, EXP2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.EXP EXP3, _, EXP3right)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, FOR1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 89.40 "tiger_pp.grm"*)Ast.For (ID, EXP1, EXP2, EXP3)(*#line 424.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, FOR1left, EXP3right), rest671)
end
|  ( 24, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 90.14 "tiger_pp.grm"*)Ast.Break(*#line 428.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, LVALUE1right)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 92.15 "tiger_pp.grm"*)Ast.Lval LVALUE(*#line 432.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, LVALUE1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RIGHT_B1right)) :: ( _, ( MlyValue.PARAM PARAM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 94.32 "tiger_pp.grm"*)Ast.FunCall (ID, PARAM)(*#line 436.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, ID1left, RIGHT_B1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RIGHT_B1right)) :: ( _, ( MlyValue.PARAM PARAM, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.EXP ((*#line 95.43 "tiger_pp.grm"*)Ast.MethodCall (LVALUE, ID, PARAM)(*#line 440.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, LVALUE1left, RIGHT_B1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 97.14 "tiger_pp.grm"*)Ast.Var ID(*#line 444.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ID ID, _, ID1right)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 98.22 "tiger_pp.grm"*)Ast.MemberRef (LVALUE, ID)(*#line 448.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, LVALUE1left, ID1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RIGHT_SQ1right)) :: ( _, ( MlyValue.EXP EXP, _, _)) :: _ :: ( _, ( MlyValue.LVALUE LVALUE, LVALUE1left, _)) :: rest671)) => let val  result = MlyValue.LVALUE ((*#line 99.36 "tiger_pp.grm"*)Ast.IdxArr (LVALUE, EXP)(*#line 452.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, LVALUE1left, RIGHT_SQ1right), rest671)
end
|  ( 31, ( rest671)) => let val  result = MlyValue.PARAM ((*#line 101.10 "tiger_pp.grm"*)[](*#line 456.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 32, ( ( _, ( MlyValue.EXP EXP, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.PARAM ((*#line 102.12 "tiger_pp.grm"*)[EXP](*#line 460.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, EXP1left, EXP1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.PARAM PARAM, _, PARAM1right)) :: _ :: ( _, ( MlyValue.EXP EXP, EXP1left, _)) :: rest671)) => let val  result = MlyValue.PARAM ((*#line 103.24 "tiger_pp.grm"*)EXP :: PARAM(*#line 464.1 "tiger_pp.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, EXP1left, PARAM1right), rest671)
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
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun LEFT_SQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun RIGHT_SQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
end
end
