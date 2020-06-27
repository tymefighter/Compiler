signature ARG = sig
    val extension   : string
    val getArg      : unit -> string * string option * string option
end

structure Arg = struct
    exception NoInputFile
    exception InputExtensionIncorrect
    exception IncorrectNumOfFlags
    exception UnRecognizedFlag

    val extension = ".tig"

    fun checkExtension fileName = 
        if (String.isSuffix extension fileName) then
            ()
        else
            raise InputExtensionIncorrect

    fun getFlags argList = 
        let
            fun getFlagsHelper 
                (flag :: fileName :: args)
                (optOutputFile, optIrFile) = (case flag of 
                    "-o" => getFlagsHelper args (SOME fileName, optIrFile)
                    | "-i" => getFlagsHelper args (optOutputFile, SOME fileName)
                    | _ => raise UnRecognizedFlag)

                | getFlagsHelper [] retVal = retVal
                | getFlagsHelper _ _ = raise IncorrectNumOfFlags

        in
            getFlagsHelper argList (NONE, NONE)
        end

    fun getArg () =
        let
            val args = CommandLine.arguments ()

            val (inputFile, otherArg) =
                case args of
                    inp :: other => (inp, other)
                    | [] => raise NoInputFile
                    
            val _ = checkExtension inputFile

            val (optOutputFile, optIrFile) = getFlags otherArg

        in
            (inputFile, optOutputFile, optIrFile)
        end
end