# Restrictions

For now, we place the following restrictions on Tree IR: -
    - BINOP can take only temporaries as input
    - MOVE can happen between mem and temp only (or temp and mem)
    - All expressions should be of the form Tree.ESEQ (stmt, resultTemp)
    - Call can only be used by the **print** function which is actually
    a systemcall

For now, we place the following restrictions on tiger code: -
    - Tiger Code needs to have a top level **let - in** block
    - The let block must contain all function declarations
    - The in block would be the "main" code of the program where the execution begins
    - There is no short-circuiting in if-statements
