structure Tokens = struct
    datatype token = AlphaStr of string
        | Symbols of string
        | Number of string
        | EOF
    end