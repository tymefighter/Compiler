structure MIPS = struct

datatype Regs = Zero
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

datatype ('l, 't) LoadStoreInst = LoadAddress of 't * int
	| LoadByte of 't * int
	| LoadByteU of 't * int
	| LoadDoubleWord of 't * int
	| LoadHalfWord of 't * int
	| LoadHalfWordU of 't * int
	| LoadWord of 't * int
	| LoadWordCoprocessor of 't * int
	| LoadWordLeft of 't * int	
	| LoadWordRight of 't * int
	| StoreByte of 't * int
	| StoreDoubleWord of 't * int
	| StoreHalfWord of 't * int
	| StoreWordCoprocessor of 't * int
	| StoreWordLeft of 't * int
	| StoreWordRight of 't * int
	| UnalignedLoadHalfword of 't * int
	| UnalignedLoadHalfwordU of 't * int
	| UnalignedLoadWord of 't * int
	| UnalignedStoreHalfword of 't * int
	| UnalignedStoreWord of 't * int

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
end
	

structure Temp = struct

	type temp = string
	type label = string

	val tempRef = ref 0
	val labelRef = ref 0
