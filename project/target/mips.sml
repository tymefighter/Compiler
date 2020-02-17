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
	| StoreWordCoprossor of 't * int
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
	| Set
	

datatype ('l, 't) Inst = LoadStore of ('l, 't) LoadStoreInst
	| ExceptionTrap of ExceptionTrapInst
	| ConstMapping of ('l, 't) ConstMappingInst
	| ArithmeticLogic of ('l, 't) ArithmeticLogicInst
end
	

	
