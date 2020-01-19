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

fun printList [] = print "\n"
    | printList (x :: xs) = let
            val _ = case x of 
                Tokens.AlphaStr s => if(Kset.member (key_list, s)) then print (TermCol.returnColorTerm TermCol.keyword s)
                                                                    else print (TermCol.returnColorTerm TermCol.id s)
                | Tokens.Symbol s => print (TermCol.returnColorTerm TermCol.symb s)
                | Tokens.Number s => print (TermCol.returnColorTerm TermCol.num s)
                | _ => ()
        in
            printList xs
        end

val _ = printList (parseCode [])