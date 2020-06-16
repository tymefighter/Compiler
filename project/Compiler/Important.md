# Restrictions

For now, we place the following restrictions on Tree IR: -
    - BINOP can take only temporaries as input
    - MOVE can happen between mem and temp only (or temp and mem)

For now, we place the following restrictions on tiger code: -
    - Tiger Code needs to have a top level **let - in** block
    - The let block must contain all function declarations
    - The in block would be the "main" code of the program where the execution begins
