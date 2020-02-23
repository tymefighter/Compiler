structure MIPS = struct

datatype Reg = Zero
	| At
	| V of int
	| A of int
	| T of int
	| S of int
	| K of int
	| Gp
	| Sp
	| Fp
	| Ra

datatype Immediate = Int of int 
	| String of string
	| Float of real

datatype Addr = RegAddr of int * Reg
	| StringAddr of string

datatype ('l, 't) LoadStoreInst = LoadAddress of 't * Addr
	| LoadByte of 't * Addr
	| LoadByteU of 't * Addr
	| LoadDoubleWord of 't * Addr
	| LoadHalfWord of 't * Addr
	| LoadHalfWordU of 't * Addr
	| LoadWord of 't * Addr
	| LoadWordCoprocessor of 't * Addr
	| LoadWordLeft of 't * Addr	
	| LoadWordRight of 't * Addr
	| StoreByte of 't * Addr
	| StoreDoubleWord of 't * Addr
	| StoreHalfWord of 't * Addr
	| StoreWord of 't * Addr
	| StoreWordCoprocessor of 't * Addr
	| StoreWordLeft of 't * Addr
	| StoreWordRight of 't * Addr
	| UnalignedLoadHalfword of 't * Addr
	| UnalignedLoadHalfwordU of 't * Addr
	| UnalignedLoadWord of 't * Addr
	| UnalignedStoreHalfword of 't * Addr
	| UnalignedStoreWord of 't * Addr

datatype ExceptionTrapInst = ReturnFromException
	| SystemCall
	| Break
	| Nop

datatype ('l, 't) ConstMappingInst = LoadImm of 't * Immediate
	| LoadUpperImm of 't * int

datatype ('l, 't) ArithmeticLogicInst = AbsVal of 't * 't
	| Add of 't * 't * 't
	| AddImm of 't * 't * Immediate
	| AddU of 't * 't * 't
	| AddImmU of 't * 't * Immediate
	| And of 't * 't * 't
	| AndImm of 't * 't * Immediate
	| Divide of 't * 't
	| DivideU of 't * 't
	| DivideQuo of 't * 't * 't
	| DivideQuoU of 't * 't * 't
	| Mul of 't * 't * 't
	| MulOver of 't * 't * 't
	| MulOverU of 't * 't * 't
	| Mult of 't * 't
	| MultU of 't * 't
	| Neg of 't * 't
	| NegU of 't * 't
	| Nor of 't * 't * 't
	| Not of 't * 't
	| Or of 't * 't * 't
	| OrImm of 't * 't * Immediate
	| Rem of 't * 't * 't
	| RotateLeft of 't * 't * 't
	| RotateRight of 't * 't * 't
	| ShiftLeftLogical of 't * 't * 't
	| ShiftLeftLogicalVar of 't * 't * 't
	| ShiftRightArithmetic of 't * 't * 't
	| ShiftRightLogical of 't * 't * 't
	| ShiftRightLogicalVar of 't * 't * 't
	| Sub of 't * 't * 't
	| SubU of 't * 't * 't
	| Xor of 't * 't * 't
	| XorImm of 't * 't * Immediate
	
datatype ('l, 't) ComparisonInst = SetEqual of 't * 't * 't
	| SetGE of 't * 't * 't
	| SetGEU of 't * 't * 't
	| SetG of 't * 't * 't
	| SetGU of 't * 't * 't
	| SetLE of 't * 't * 't
	| SetLEU of 't * 't * 't
	| SetL of 't * 't * 't
	| SetLImm of 't * 't * Immediate
	| SetLU of 't * 't * 't
	| SetLImmU of 't * 't * Immediate
	| SetNotEqual of 't * 't * 't

datatype ('l, 't) BranchJumpInst = Branch of 'l
	| BranchCoprocessorZTrue of 'l
	| BranchCoprocessorZFalse of 'l
	| BranchEq of 't * 't * 'l
	| BranchEqZero of 't * 'l
	| BranchGE of 't * 't * 'l
	| BranchGEU of 't * 't * 'l
	| BranchGEZero of 't * 'l
	| BranchGEZeroLink of 't * 'l
	| BranchG of 't * 't * 'l
	| BranchGU of 't * 't * 'l
	| BranchGZero of 't * 'l
	| BranchLE of 't * 't * 'l
	| BranchLEU of 't * 't * 'l
	| BranchLEZero of 't * 'l
	| BranchLessZeroLink of 't * 'l
	| BranchL of 't * 't * 'l
	| BranchLU of 't * 't * 'l
	| BranchLZero of 't * 'l
	| BranchNE of 't * 't * 'l
	| BranchNEZero of 't * 'l
	| Jump of 'l
	| JumpLink of 'l
	| JumpLinkReg of 't
	| JumpReg of 't
	
datatype ('l, 't) DataMoveInst = Move of 't * 't
	| MoveFormHI of 't
	| MoveFromLO of 't
	| MoveToHI of 't
	| MoveToLO of 't

datatype ('l, 't) Inst = LoadStore of ('l, 't) LoadStoreInst
	| ExceptionTrap of ExceptionTrapInst
	| ConstMapping of ('l, 't) ConstMappingInst
	| ArithmeticLogic of ('l, 't) ArithmeticLogicInst
	| Comparison of ('l, 't) ComparisonInst
	| BranchJump of ('l, 't) BranchJumpInst
	| DataMove of ('l, 't) DataMoveInst
	
	
	fun prettyReg Zero = "$zero"
		| prettyReg At = "$at"
		| prettyReg (V n) = "$v" ^ Int.toString n
		| prettyReg (A n) = "$a" ^ Int.toString n
		| prettyReg (T n) = "$t" ^ Int.toString n
		| prettyReg (S n) = "$s" ^ Int.toString n
		| prettyReg (K n) = "$k" ^ Int.toString n
		| prettyReg Gp = "$gp" 
		| prettyReg Sp = "$sp"
		| prettyReg Fp = "$fp"
		| prettyReg Ra = "$ra"
	
	fun prettyImm (Int n) = Int.toString n
		| prettyImm (String s) = s
		| prettyImm (Float f) = Real.toString f

	fun prettyAddr (RegAddr (off, reg)) = Int.toString off ^ "(" ^ prettyReg reg ^ ")"
		| prettyAddr (StringAddr addr) = addr
	
	fun prettyLoadStore (LoadAddress (reg, addr)) = "la " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadByte (reg, addr)) = "lb " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadByteU (reg, addr)) = "lbu " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadDoubleWord (reg, addr)) = "ld " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadHalfWord (reg, addr)) = "lh " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadHalfWordU (reg, addr)) = "lhu " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadWord (reg, addr)) = "lw " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadWordCoprocessor (reg, addr)) = "lwcz " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadWordLeft (reg, addr)) = "lwl " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (LoadWordRight (reg, addr)) = "lwr " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreByte (reg, addr)) = "sb " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreDoubleWord (reg, addr)) = "sd " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreHalfWord (reg, addr)) = "sh " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreWord (reg, addr)) = "sw " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreWordCoprocessor (reg, addr)) = "swcz " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreWordLeft (reg, addr)) = "swl " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (StoreWordRight (reg, addr)) = "swr " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (UnalignedLoadHalfword (reg, addr)) = "ulh " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (UnalignedLoadHalfwordU (reg, addr)) = "ulhu " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (UnalignedLoadWord (reg, addr)) = "ulw " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (UnalignedStoreHalfword (reg, addr)) = "ush " ^ prettyReg reg ^ ", " ^ prettyAddr addr
		| prettyLoadStore (UnalignedStoreWord (reg, addr)) = "usw " ^ prettyReg reg ^ ", " ^ prettyAddr addr

	fun prettyExceptionTrap ReturnFromException = "rfe"
		| prettyExceptionTrap SystemCall = "syscall"
		| prettyExceptionTrap Break = "break"
		| prettyExceptionTrap Nop = "nop"
	
	fun prettyConstMapping (LoadImm (rDest, imm)) = "li " ^ prettyReg rDest ^ ", " ^ prettyImm imm
		| prettyConstMapping (LoadUpperImm (rDest, n)) = "lui " ^ prettyReg rDest ^ ", " ^ Int.toString n

	fun prettyArithmeticLogic (AbsVal (rDest, rSource)) = "abs " ^ prettyReg rDest ^ ", " ^ prettyReg rSource
		| prettyArithmeticLogic (Add (rDest, rSource1, rSource2)) = "add " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (AddImm (rDest, rSource, imm)) = "addi " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyArithmeticLogic (AddU (rDest, rSource1, rSource2)) = "addu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (AddImmU (rDest, rSource, imm)) = "addiu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyArithmeticLogic (And (rDest, rSource1, rSource2)) = "and " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (AndImm (rDest, rSource, imm)) = "andi " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyArithmeticLogic (Divide (rSource1, rSource2)) = "div " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (DivideU (rSource1, rSource2)) = "divu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (DivideQuo (rDest, rSource1, rSource2)) = "div " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (DivideQuoU (rDest, rSource1, rSource2)) = "divu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Mul (rDest, rSource1, rSource2)) = "mul " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (MulOver (rDest, rSource1, rSource2)) = "mulo " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (MulOverU (rDest, rSource1, rSource2)) = "mulou " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Mult (rSource1, rSource2)) = "mult " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (MultU (rSource1, rSource2)) = "multu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Neg (rDest, rSource)) = "neg " ^ prettyReg rDest ^ ", " ^ prettyReg rSource
		| prettyArithmeticLogic (NegU (rDest, rSource)) = "negu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource
		| prettyArithmeticLogic (Nor (rDest, rSource1, rSource2)) = "nor " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Not (rDest, rSource)) = "not " ^ prettyReg rDest ^ ", " ^ prettyReg rSource
		| prettyArithmeticLogic (Or (rDest, rSource1, rSource2)) = "or " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (OrImm (rDest, rSource, imm)) = "ori " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyArithmeticLogic (Rem (rDest, rSource1, rSource2)) = "rem " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (RotateLeft (rDest, rSource1, rSource2)) = "rol " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (RotateRight (rDest, rSource1, rSource2)) = "ror " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (ShiftLeftLogical (rDest, rSource1, rSource2)) = "sll " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (ShiftLeftLogicalVar (rDest, rSource1, rSource2)) = "sllv " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (ShiftRightArithmetic (rDest, rSource1, rSource2)) = "sra " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (ShiftRightLogical (rDest, rSource1, rSource2)) = "srl " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (ShiftRightLogicalVar (rDest, rSource1, rSource2)) = "srlv " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (SubU (rDest, rSource1, rSource2)) = "sub " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Sub (rDest, rSource1, rSource2)) = "subu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (Xor (rDest, rSource1, rSource2)) = "xor " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyArithmeticLogic (XorImm (rDest, rSource, imm)) = "xori " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
	
	fun prettyComparison (SetEqual (rDest, rSource1, rSource2)) = "seq " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetGE (rDest, rSource1, rSource2)) = "sge " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetGEU (rDest, rSource1, rSource2)) = "sgeu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetG (rDest, rSource1, rSource2)) = "sgt " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetGU (rDest, rSource1, rSource2)) = "sgtu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2
		| prettyComparison (SetLE (rDest, rSource1, rSource2)) = "sle " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetLEU (rDest, rSource1, rSource2)) = "sleu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetL (rDest, rSource1, rSource2)) = "slt " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetLImm  (rDest, rSource, imm)) = "slti " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyComparison (SetLU (rDest, rSource1, rSource2)) = "sltu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 
		| prettyComparison (SetLImmU  (rDest, rSource, imm)) = "sltiu " ^ prettyReg rDest ^ ", " ^ prettyReg rSource ^ ", " ^ prettyImm imm
		| prettyComparison (SetNotEqual (rDest, rSource1, rSource2)) = "sne " ^ prettyReg rDest ^ ", " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 

	fun prettyBranchJump (Branch label) = "b " ^ label
		| prettyBranchJump (BranchCoprocessorZTrue label) = "bczt " ^ label
		| prettyBranchJump (BranchCoprocessorZFalse label) = "bczf " ^ label
		| prettyBranchJump (BranchEq (rSource1, rSource2, label)) = "beq " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchEqZero (rSource, label)) = "beqz " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchGE (rSource1, rSource2, label)) = "bge " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchGEU (rSource1, rSource2, label)) = "bgeu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchGEZero (rSource, label)) = "bgez " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchGEZeroLink (rSource, label)) = "bgezal " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchG (rSource1, rSource2, label)) = "bgt " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchGU (rSource1, rSource2, label)) = "bgtu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchGZero (rSource, label)) = "bgtz " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchLE (rSource1, rSource2, label)) = "ble " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchLEU (rSource1, rSource2, label)) = "bleu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchLEZero (rSource, label)) = "blez " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchLessZeroLink (rSource, label)) = "bltzal " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchL (rSource1, rSource2, label)) = "blt " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchLU (rSource1, rSource2, label)) = "bltu " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchLZero (rSource, label)) = "bltz " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (BranchNE (rSource1, rSource2, label)) = "bne " ^ prettyReg rSource1 ^ ", " ^ prettyReg rSource2 ^ ", " ^ label
		| prettyBranchJump (BranchNEZero (rSource, label)) = "bnez " ^ prettyReg rSource ^ ", " ^ label
		| prettyBranchJump (Jump label) = "j " ^ label
		| prettyBranchJump (JumpLink label) = "jal " ^ label
		| prettyBranchJump (JumpLinkReg rSource) = "jalr " ^ prettyReg rSource
		| prettyBranchJump (JumpReg rSource) = "jr " ^ prettyReg rSource
	
	fun prettyDataMove (Move (rDest, rSource)) = "move " ^ prettyReg rDest ^ ", " ^ prettyReg rSource
		| prettyDataMove (MoveFormHI rDest) = "mfhi " ^ prettyReg rDest 
		| prettyDataMove (MoveFromLO rDest) = "mflo " ^ prettyReg rDest
		| prettyDataMove (MoveToHI rDest) = "mthi " ^ prettyReg rDest
		| prettyDataMove (MoveToLO rDest) = "mtlo " ^ prettyReg rDest

	fun prettyInst (LoadStore load_store_inst) = prettyLoadStore load_store_inst
		| prettyInst (ExceptionTrap exc_trap_inst) = prettyExceptionTrap exc_trap_inst
		| prettyInst (ConstMapping const_map_inst) = prettyConstMapping const_map_inst
		| prettyInst (ArithmeticLogic ar_logic_inst) = prettyArithmeticLogic ar_logic_inst
		| prettyInst (Comparison comp_inst) = prettyComparison comp_inst
		| prettyInst (BranchJump br_jmp_inst) = prettyBranchJump br_jmp_inst
		| prettyInst (DataMove dt_mv_inst) = prettyDataMove dt_mv_inst
end

signature TEMP = sig
    type temp
    type label
	val newtemp  : unit -> temp
    val newlabel : unit -> label
end
	

structure Temp : TEMP = struct

	type temp = string
	type label = string

	val tempRef = ref ""
	val labelRef = ref ""

	fun getNewString [c] = (
			if (Char.toString c) = "z" then
				["z", "a"]
			else
				[Char.toString (Char.succ c)]
		)
		| getNewString (ch :: chs) = (Char.toString ch) :: getNewString chs
		| getNewString [] = ["a"]


	fun newtemp () = let
			val new_str = (String.concat o getNewString o String.explode) (!tempRef)
			val _ = tempRef := new_str
		in
			new_str
		end
	
	fun newlabel () = let
			val new_str = (String.concat o getNewString o String.explode) (!labelRef)
			val _ = labelRef := new_str
		in
			new_str
		end
end