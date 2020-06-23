# Restrictions

For now, we place the following restrictions on Tree IR: -
    - BINOP can take only temporaries as input
    - MOVE can happen between mem and temp only (or temp and mem), and
    in some cases from binop between temps to temp.
    - All expressions should be of the form Tree.ESEQ (stmt, resultTemp)
    - Call can only be used by the **print** and **println** functions
    for actual function call, while it can be used by other functions
    for just jumping and linking, **we must pass this without arguments**
    - Allocate statement **should not** use result temp
    - Function call assumes that the frame pointer points to an previous
    frame pointer value which is stored in the stack, the next location
    contains the return address and the next few locations contain the local
    variables. The stack frame is organized as follows: -

                    |---------------|
    Frame pointer-->|  prev fp val  | 
                    |  return addr  | 
                    |     arg1      |
                    |     arg2      |
                    |     arg3      |
                    |     ....      |
                    |   Local var   |
    Stack pointer-->|     ....      |
    
    - We can return the return values in the return value register
    - We have a major assumption - **no errors in the code**

For now, we place the following restrictions on tiger code: -
    - Tiger Code needs to have a top level **let - in** block
    - The let block must contain all function declarations (and only function
    declarations).
    - We do not support nested functions as of now.
    - The in block would be the "main" code of the program where the execution begins
    - There is no short-circuiting in if-statements
