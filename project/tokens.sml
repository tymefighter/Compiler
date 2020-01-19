structure Tokens = struct
    datatype token = AlphaStr of string
        | Symbol of string
        | Number of string
        | EOF
    end