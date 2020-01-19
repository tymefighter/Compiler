structure TermCol = struct

    val esc = "\027"
    val blue = esc ^ "[34m"
    val red = esc ^ "[31m"
    val green = esc ^ "[32m"
    val yellow = esc ^ "[33m"
    val magenta = esc ^ "[35m"
    val cyan = esc ^ "[36m"
    val noColorEnd = esc ^ "[0m"

    datatype term = keyword | symb | id | num | comment | string

    fun returnColorTerm keyword s = blue ^ s ^ noColorEnd
        | returnColorTerm symb s = red ^ s ^ noColorEnd
        | returnColorTerm id s = green ^ s ^ noColorEnd
        | returnColorTerm num s = yellow ^ s ^ noColorEnd
        | returnColorTerm comment s = magenta ^ s ^ noColorEnd
        | returnColorTerm string s = cyan ^ s ^ noColorEnd
    
end