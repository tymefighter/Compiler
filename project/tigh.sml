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
    "in", "end", "of", "break", "nil", "function", "var", "type", "import", "primitive"]

structure StringKey = struct
    type ord_key = string
    val compare = String.compare
end

structure Kset = BinarySetFn(StringKey)
val key_list = Kset.addList (Kset.empty, keywords_arr) (* Get the keywords array in the form of a list for efficient lookup *)