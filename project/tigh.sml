(* Building the lexer on standard input *)
val lexer = TigerLex.makeLexer (fn n => TextIO.inputN (TextIO.stdIn, n))

(* 
    A function to parse the tiger code and return a list of all
    tokens found in the exact order they were found
*)

fun parseCode ls = let
    val tok = lexer ()
    in
        if(tok = Tokens.EOF) then ls
        else parseCode (ls @ [tok])
    end


val keywords_arr = ["array", "if", "then", "else", "while", "for", "to", "do", "let",
    "in", "end", "of", "break", "nil", "function", "var", "type", "import", "primitive",
    "class", "extends", "method", "new"]

structure StringKey = struct
    type ord_key = string
    val compare = String.compare
end

structure Kset = BinarySetFn(StringKey)
val key_list = Kset.addList (Kset.empty, keywords_arr) (* Get the keywords array in the form of a list for efficient lookup *)

fun printSpaces s 0 = s
    | printSpaces s n = printSpaces (s ^ " ") (n - 1) 

fun printSpaceAndLine line_no pos_from_prev prev_line = let

        val upd_prev_line = if(line_no <> prev_line) then (let
                    val _ = print "\n"
                in
                    prev_line + 1
                end)
                else
                    prev_line

        val _ = printSpaces "" pos_from_prev 
    in
        upd_prev_line
    end

fun printTok (Tokens.AlphaStr (s, line_no, pos_from_prev)) prev_line = let
        
        val upd_prev_line = printSpaceAndLine line_no pos_from_prev prev_line
        val _ = if(Kset.member (key_list, s)) then print (TermCol.returnColorTerm TermCol.keyword s)
            else print (TermCol.returnColorTerm TermCol.id s)
    in
        upd_prev_line
    end
    | printTok (Tokens.Symbol (s, line_no, pos_from_prev)) prev_line = let

        val upd_prev_line = printSpaceAndLine line_no pos_from_prev prev_line
        val _ = print (TermCol.returnColorTerm TermCol.symb s)
    in
        upd_prev_line
    end
    | printTok (Tokens.Number (s, line_no, pos_from_prev)) prev_line = let
        val upd_prev_line = printSpaceAndLine line_no pos_from_prev prev_line
        val _ = print (TermCol.returnColorTerm TermCol.num s)
    in
        upd_prev_line
    end
    | printTok Tokens.EOF _ = 0

fun printList [] prev_line = print "\n"
    | printList (x :: xs) prev_line = let
            val upd_prev_line = printTok x prev_line
        in
            printList xs upd_prev_line
        end

val _ = printList (parseCode []) 0