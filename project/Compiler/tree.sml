structure Tree = struct

    (* 
        Very Important: -
        For now, we would be doing as follows: -
            - BINOP can take only temporaries as input
            - MOVE can happen between mem and temp only (or temp and mem)
    *)

    datatype exp = CONST of int
        | NAME of Temp.label
        | TEMP of Temp.temp
        | BINOP of binop * exp * exp
        | MEM of exp
        | CALL of exp * exp list
        | ESEQ of stm * exp
    
    and stm = MOVE of exp * exp
        | EXP of exp
        | JUMP of exp * Temp.label list
        | CJUMP of relop * exp * exp * Temp.label * Temp.label
        | SEQ of stm * stm
        | LABEL of Temp.label

    and binop = PLUS | MINUS | MUL | DIV
        | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
    
    and relop = EQ | NE | LT | GT | LE | GE
        | ULT | ULE | UGT | UGE
    
    exception Incomplete

    fun pprintRelop EQ = "EQ"
        | pprintRelop NE = "NE"
        | pprintRelop LT = "LT"
        | pprintRelop GT = "GT"
        | pprintRelop LE = "LE"
        | pprintRelop GE = "GE"
        | pprintRelop ULT = "ULT"
        | pprintRelop ULE = "ULE"
        | pprintRelop UGT = "UGT"
        | pprintRelop UGE = "UGE"

    fun pprintExp (CONST n) = Int.toString n
        | pprintExp (NAME lab) = "NAME " ^ Temp.labelToString lab
        | pprintExp (TEMP tmp) = "Temp " ^ Temp.tempToString tmp
        | pprintExp (BINOP (bin_op, e1, e2)) = let
                val bin_op_str = case bin_op of 
                    PLUS => "PLUS "
                    | MINUS => "MINUS "
                    | MUL => "MUL "
                    | DIV => "DIV "
                    | AND => "AND "
                    | OR => "OR "
                    | LSHIFT => "LSHIFT "
                    | RSHIFT => "RSHIFT "
                    | ARSHIFT => "ARSHIFT "
                    | XOR => "XOR "
            in
                bin_op_str ^ "(" ^ pprintExp e1 ^ ", " ^ pprintExp e2 ^ ")"
            end
        | pprintExp (MEM e) = "Mem (" ^ pprintExp e ^ ")"
        | pprintExp (CALL (f_name, arg_list)) = let
                fun printArg [arg] = pprintExp arg
                    | printArg (arg :: arg_ls) = pprintExp arg ^ ", " ^ printArg arg_ls
                    | printArg [] = ""
            in
                pprintExp f_name ^ " (" ^ printArg arg_list ^ ")"
            end
        | pprintExp (ESEQ (st, ex)) = "ESEQ (" ^ pprintStm st ^ ", " ^ pprintExp ex ^ ")"
    
    and pprintStm (MOVE (to, value)) = "MOVE (" ^ pprintExp to ^ ", " ^ pprintExp value ^ ")"
        | pprintStm (EXP ex) = "EXP (" ^ pprintExp ex ^ ")"
        | pprintStm (JUMP (loc, _)) = "JUMP (" ^ pprintExp loc ^ ")"
        | pprintStm (CJUMP (rel_op, e1, e2, true_lab, false_lab)) = "CJUMP (" ^ pprintRelop rel_op ^ ", " ^ pprintExp e1 ^ ", " ^ pprintExp e2 ^ ", " ^ Temp.labelToString true_lab ^ ", " ^ Temp.labelToString false_lab ^ ")" 
        | pprintStm (SEQ (st1, st2)) = "SEQ (" ^ pprintStm st1 ^ ", " ^ pprintStm st2 ^ ")"
        | pprintStm (LABEL lab) = "Label (" ^ Temp.labelToString lab ^ ")"
end