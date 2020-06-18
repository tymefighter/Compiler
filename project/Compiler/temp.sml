signature TEMP = sig
    type temp
    type label
	val newtemp  : unit -> temp
    val newlabel : unit -> label
    val stringToLabel : string -> label
	val labelToString : label -> string
	val tempToString : temp -> string
    val argTemp1 : temp
    val argTemp2 : temp
    val resultTemp : temp
    val framePointer : temp
    val stackPointer : temp
end	

structure Temp :> TEMP = struct

	type temp = string
	type label = string

	val tempRef = ref ""
	val labelRef = ref ""

    fun getNewString char_list = let
            fun getNewStringHelper [c] = (
                if(Char.toString c) = "z" then
                    (["a"], true)
                else
                    ([Char.toString (Char.succ c)], false)
            )
            | getNewStringHelper (ch :: chs) = let
                    val (str_ls, update_curr) = getNewStringHelper chs
                in
                    if(update_curr) then (
                        if(Char.toString ch) = "z" then
                            (("a") :: str_ls, true)
                        else
                            ((Char.toString (Char.succ ch)) :: str_ls, false)
                    )
                    else
                        ((Char.toString ch) :: str_ls, false)
                end
            | getNewStringHelper [] = (["a"], false)
            
            val (str_ls, upd_first) = getNewStringHelper char_list
        in
            if(upd_first) then
                "a" :: str_ls
            else
                str_ls
        end


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
			"L_" ^ new_str
		end

    fun stringToLabel str = str

	fun labelToString lb = lb
	fun tempToString tp = tp

    val argTemp1 = "argTemp1"
    val argTemp2 = "argTemp2"
    val resultTemp = "resultTemp"
    val framePointer = "framePointer"
    val stackPointer = "stackPointer"
end
