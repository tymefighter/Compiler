type ('l, 't) Prog = ('l option * ('l, 't) MIPS.Inst) list

fun printProg ((label_opt, inst) :: pr_ls) = let
            val str1 = case label_opt of
                SOME lab => lab ^ ": "
                | NONE => ""
            val str2 = MIPS.prettyInst inst ^ "\n" ^ printProg pr_ls
        in
            str1 ^ str2
        end
    | printProg _ = "\n"

(* Test Cases *)

(* 
    TC 1: Basic systemcall

    main: addi $v0, $zero, 4
	la	$a0, msg
	syscall
	addi $v0, $zero, 10
	syscall
*)
val _ = print ("TC1 ------------------------------------\n")
val prog1 : (string, MIPS.Reg) Prog = [
    (SOME "main", MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 4))),
    (NONE, MIPS.LoadStore (MIPS.LoadAddress (MIPS.A 0, MIPS.StringAddr "msg"))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 10))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall))
]

val _ = print (printProg prog1)

(* 
    TC 2: Function + Syscall

    main: lw $s0, x
	lw $s1, y
	move $a0, $s0
	jal	fun
	move $s1,$v0
	addi $v0, $zero, 4
	la $a0, msg1
	syscall
	addi $v0, $zero, 1
	move $a0, $s1
	syscall
	addi $v0, $zero, 4
	la $a0, lf
	syscall
	addi $v0, $zero, 10
	syscall
    fun: addi $s0, $zero, 3
	mul $s1, $s0, $a0
	addi $s1, $s1, 5
	move $v0, $s1
	jr $ra	
*)
val _ = print ("TC2 ------------------------------------\n")
val prog2 : (string, MIPS.Reg) Prog = [
    (SOME "main", MIPS.LoadStore (MIPS.LoadWord (MIPS.S 0, MIPS.StringAddr "x"))),
    (NONE, MIPS.LoadStore (MIPS.LoadWord (MIPS.S 1, MIPS.StringAddr "y"))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.A 0, MIPS.V 0))),
    (NONE, MIPS.BranchJump (MIPS.JumpLink "fun")),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.S 1, MIPS.V 0))),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 4))),
    (NONE, MIPS.LoadStore (MIPS.LoadAddress (MIPS.A 0, MIPS.StringAddr "msg1"))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 1))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.A 0, MIPS.S 1))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 4))),
    (NONE, MIPS.LoadStore (MIPS.LoadAddress (MIPS.A 0, MIPS.StringAddr "lf"))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 10))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (SOME "fun", MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.S 0, MIPS.Zero, MIPS.Int 3))),
    (NONE, MIPS.ArithmeticLogic (MIPS.Mul (MIPS.S 1, MIPS.S 0, MIPS.A 0))),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.S 1, MIPS.S 1, MIPS.Int 5))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.V 0, MIPS.S 1))),
    (NONE, MIPS.BranchJump (MIPS.JumpReg MIPS.Ra))
]

val _ = print (printProg prog2)

(* 
    TC 3: Loop + Syscall

    main: la $t0, arr
    move $t1, $zero
    addi $t2, $zero, 10
    move $t3, $zero

    loop: beq $t1, $t2, done
    lw $t4, 0($t0)
    add $t3, $t3, $t4
    addi $t0, $t0, 4
    addi $t1, $t1, 1
    j loop

    done: addi $v0, $zero, 1
    move $a0, $t3
    syscall
    addi $v0, $zero, 10
	syscall
*)

val _ = print ("TC3 ------------------------------------\n")
val prog3 : (string, MIPS.Reg) Prog = [
    (SOME "main", MIPS.LoadStore (MIPS.LoadAddress (MIPS.T 0, MIPS.StringAddr "arr"))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.T 1, MIPS.Zero))),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.T 2, MIPS.Zero, MIPS.Int 10))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.T 3, MIPS.Zero))),
    (SOME "loop", MIPS.BranchJump (MIPS.BranchEq (MIPS.T 1, MIPS.T 2, "done"))),
    (NONE, MIPS.LoadStore (MIPS.LoadWord (MIPS.T 4, MIPS.RegAddr (0, MIPS.T 0)))),
    (NONE, MIPS.ArithmeticLogic (MIPS.Add (MIPS.T 3, MIPS.T 3, MIPS.T 4))),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.T 0, MIPS.T 0, MIPS.Int 4))),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.T 1, MIPS.T 1, MIPS.Int 1))),
    (NONE, MIPS.BranchJump (MIPS.Jump "loop")),
    (SOME "done", MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 1))),
    (NONE, MIPS.DataMove (MIPS.Move (MIPS.A 0, MIPS.T 3))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall)),
    (NONE, MIPS.ArithmeticLogic (MIPS.AddImm (MIPS.V 0, MIPS.Zero, MIPS.Int 10))),
    (NONE, MIPS.ExceptionTrap (MIPS.SystemCall))
]

val _ = print (printProg prog3)