structure Tokens = struct
    datatype token = AlphaStr of string * int * int
        | Symbol of string * int * int
        | Number of string * int * int
        | EOF
    end