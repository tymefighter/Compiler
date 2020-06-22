structure CodeGen = struct

    exception UnimplemenetedCodeGen
    exception RestrictionFailedCodeGen

    fun temp2reg temp = case Temp.tempToString temp of
        "argTemp1" => MIPS.T 1
        | "argTemp2" => MIPS.T 2
        | "resultTemp" => MIPS.T 0
        | "framePointer" => MIPS.Fp
        | "stackPointer" => MIPS.Sp
        | _ => raise RestrictionFailedCodeGen

    fun mapNoLabel instList = map (fn inst => MIPS.Instruction inst) instList

    (* generateStm : Tree.stm -> ((MIPS.Reg * MIPS.Addr) MIPS.Inst) list *)
    fun generateStm (Tree.MOVE moveExp) =
        let
            val mipsStmt = case moveExp of
                (Tree.TEMP temp1, Tree.TEMP temp2) => MIPS.DataMove (MIPS.Move (temp2reg temp1, temp2reg temp2))
                | (Tree.TEMP temp, Tree.CONST c)   => MIPS.ConstMapping (MIPS.LoadImm (temp2reg temp, MIPS.Int c))
                | (Tree.TEMP resTemp, Tree.BINOP (bin_op, Tree.TEMP argTemp1, Tree.TEMP argTemp2)) =>
                    let
                        val arg1      = temp2reg argTemp1
                        val arg2      = temp2reg argTemp2
                        val resultReg = temp2reg resTemp

                        val instConstructor = case bin_op of
                            Tree.PLUS      => MIPS.Add
                            | Tree.MINUS   => MIPS.Sub
                            | Tree.MUL     => MIPS.Mul
                            | Tree.DIV     => MIPS.DivideQuo
                            | Tree.AND     => MIPS.And
                            | Tree.OR      => MIPS.Or
                            | Tree.LSHIFT  => MIPS.ShiftLeftLogical
                            | Tree.RSHIFT  => MIPS.ShiftRightLogical
                            | Tree.ARSHIFT => MIPS.ShiftRightArithmetic
                            | Tree.XOR     => MIPS.Xor
                    in
                        MIPS.ArithmeticLogic (instConstructor (resultReg, arg1, arg2))
                    end
                | (Tree.TEMP resTemp, Tree.MEM mem) =>
                    let
                        val (r_offset, reg) = case mem of
                            Tree.BINOP (Tree.PLUS, Tree.TEMP temp, Tree.CONST r_off) => (r_off, temp2reg temp)
                            | _ => raise RestrictionFailedCodeGen

                        val resReg = temp2reg resTemp
                    in
                        MIPS.LoadStore (MIPS.LoadWord (resReg, MIPS.RegAddr (r_offset, reg)))
                    end
                | (Tree.MEM mem, Tree.TEMP tempFrom) =>
                    let
                        val (r_offset, reg) = case mem of
                            Tree.BINOP (Tree.PLUS, Tree.TEMP temp, Tree.CONST r_off) => (r_off, temp2reg temp)
                            | _ => raise RestrictionFailedCodeGen

                        val regFrom = temp2reg tempFrom
                    in
                        MIPS.LoadStore (MIPS.StoreWord (regFrom, MIPS.RegAddr (r_offset, reg)))
                    end
                | _ => raise RestrictionFailedCodeGen
        in
            mapNoLabel [mipsStmt]
        end
        | generateStm (Tree.JUMP (jumpExp, labelList)) = 
            let
                val jumpLabelStr = case jumpExp of
                    (Tree.NAME label) => Temp.labelToString label
                    | _ => raise RestrictionFailedCodeGen
            in
                mapNoLabel [MIPS.BranchJump (MIPS.Jump jumpLabelStr)]
            end
        | generateStm (Tree.CJUMP (relOp, Tree.TEMP argTemp1, Tree.TEMP argTemp2, trueLabel, falseLabel)) =
            let
                val reg1 = temp2reg argTemp1
                val reg2 = temp2reg argTemp2
                
                val trueLabelStr = Temp.labelToString trueLabel
                val falseLabelStr = Temp.labelToString falseLabel

                val instConstructor = case relOp of
                    Tree.EQ    => MIPS.BranchEq
                    | Tree.NE  => MIPS.BranchNE
                    | Tree.LT  => MIPS.BranchL
                    | Tree.GE  => MIPS.BranchGE
                    | Tree.GT  => MIPS.BranchG
                    | Tree.LE  => MIPS.BranchLE
                    | Tree.ULT => MIPS.BranchLU
                    | Tree.ULE => MIPS.BranchLEU
                    | Tree.UGT => MIPS.BranchGU
                    | Tree.UGE => MIPS.BranchGEU
            in
                mapNoLabel [
                    MIPS.BranchJump (instConstructor (reg1, reg2, trueLabelStr)),
                    MIPS.BranchJump (MIPS.Jump falseLabelStr)
                ]
            end
        | generateStm (Tree.SEQ (stm1, stm2)) = generateStm stm1 @ generateStm stm2
        | generateStm (Tree.LABEL label) = [MIPS.Label (Temp.labelToString label)]
        | generateStm (Tree.EXP (Tree.CALL (Tree.NAME lab, argList))) =
            (case argList of
                [Tree.TEMP temp] =>
                    let
                        val funcName = Temp.labelToString lab
                        val printlnStmts = case funcName of
                            "print" => []
                            | "println" => [
                                    MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.A 0, MIPS.Zero, MIPS.Int 10)),
                                    MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 11)),
                                    MIPS.ExceptionTrap (MIPS.SystemCall)
                                ]
                            | _ => raise RestrictionFailedCodeGen
                        
                        val printReg = temp2reg temp
                    in
                        mapNoLabel ([
                            MIPS.ConstMapping (MIPS.LoadImm (MIPS.V 0, MIPS.Int 1)),
                            MIPS.DataMove (MIPS.Move (MIPS.A 0, printReg)),
                            MIPS.ExceptionTrap (MIPS.SystemCall)
                        ] @ printlnStmts)
                    end
                | [] =>
                    let
                        val funcName = Temp.labelToString lab
                        val mipsStmt = MIPS.BranchJump (MIPS.JumpLink funcName)
                    in
                        mapNoLabel [mipsStmt]
                    end
                | _ => raise RestrictionFailedCodeGen)
        | generateStm _ = raise RestrictionFailedCodeGen

    (* generateEx : Tree.exp -> ((MIPS.Reg * MIPS.Addr) MIPS.Inst) list *)
    fun generateProg (Tree.ESEQ (stm, ex)) = 
           MIPS.Instruction (MIPS.DataMove (MIPS.Move (MIPS.Fp, MIPS.Sp))) :: generateStm stm
        | generateProg _ = raise RestrictionFailedCodeGen
end