structure TermCol = struct

    val esc = CHAR.chr 27
    val blue = esc ^ "[34m"
    val red = esc ^ "[31m"
    val green = esc ^ "[32m"
    val yellow = esc ^ "[33m"
    val noColor = esc ^ "[0m"

    fun printTerm (Tokens.AlphaStr s) = blue ^ s ^ noColor
        | (Tokens.Symbols s) = red ^ s ^ noColor
        | (Tokens.Number s) = green ^ s ^ noColor
        | _ = ""