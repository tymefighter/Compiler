type Symbol = Atom.atom
type Token  = Atom.atom

type Symbols = AtomSet.set
type Tokens = AtomSet.set

type RHS = Atom.atom list  (* The RHS γ of a rule A -> γ *)

structure RHS_KEY : ORD_KEY = struct
    type ord_key = RHS
    val compare = List.collate Atom.compare
end

structure RHSSet = RedBlackSetFn (RHS_KEY)

type Productions = RHSSet.set

type Rules = Productions AtomMap.map

type Grammar = { symbols : Symbols, tokens : Tokens, rules : Rules }

(* Algorithm *)

fun getNullableForProd nullable symbols sym [] = AtomSet.add (nullable, sym)
    | getNullableForProd nullable symbols sym (x :: xs) = (
        if(AtomSet.member (symbols, x) andalso AtomSet.member (nullable, x)) then 
            getNullableForProd nullable symbols sym xs
        else
            nullable
    )

fun getFirstForProd first symbols nullable sym [] = first
    | getFirstForProd first symbols nullable sym (x :: xs) = let
            
            val firstSymOpt = AtomMap.find (first, sym)
            val (first_1, firstSym) = (
                case firstSymOpt of 
                    NONE => (AtomMap.insert (first, sym, AtomSet.empty), AtomSet.empty) (* first does not contain sym, so insert it *)
                    | SOME (first_sym) => (first, first_sym)                            (* first contains sym, so return its first set *)
            )

            val first_new = if(AtomSet.member (symbols, x)) then
                    let
                        val firstxOpt = AtomMap.find (first_1, x)
                        val (first_2, firstx) = (
                            case firstxOpt of
                                NONE => (AtomMap.insert (first_1, x, AtomSet.empty), AtomSet.empty) (* first does not contain x, so insert it *)
                                | SOME (first_x) => (first_1, first_x)                              (* first contains x, so return its first set *)
                        )
                    in
                        AtomMap.insert (first_2, sym, AtomSet.union (firstSym, firstx))
                    end
                else
                    AtomMap.insert (first_1, sym, AtomSet.add (firstSym, x))

        in
            if(AtomSet.member (symbols, x) andalso AtomSet.member (nullable, x)) then
                getFirstForProd first_new symbols nullable sym xs
            else
                first_new
        end

fun listStr [] = "\n"
    | listStr (x :: xs) = x ^ " " ^ listStr xs

(* Imp: The RHS list has been reversed and passed here for efficient computation of first and nullable of suffixes of actual RHS list *)
fun getFollowForProd first follow symbols nullable sym first_suff is_nullable_suff [] = follow 
    | getFollowForProd first follow symbols nullable sym first_suff is_nullable_suff (rev_x :: rev_xs) = let

        val followSymOpt = AtomMap.find (follow, sym)
        val (follow_1, followSym) = (
            case followSymOpt of 
                NONE => (AtomMap.insert (follow, sym, AtomSet.empty), AtomSet.empty)   (* follow does not contain sym, so insert it *)
                | SOME (follow_sym) => (follow, follow_sym)                            (* follow contains sym, so return its follow set *)
        )

        (* Mistake !!*)
        val follow_new = if(AtomSet.member (symbols, rev_x)) then
                let
                    val follow_rev_xOp = AtomMap.find (follow_1, rev_x)
                    val (follow_2, follow_rev_x) = (
                        case follow_rev_xOp of
                            NONE => (AtomMap.insert (follow_1, rev_x, AtomSet.empty), AtomSet.empty)  (* follow does not contain rev_x, so insert it *)
                            | SOME (follow_rev_x_found) => (follow_1, follow_rev_x_found)             (* follow contains rev_x, so return its follow set *)
                    )

                    val follow_3 = if(is_nullable_suff) then 
                            AtomMap.insert (follow_2, rev_x, AtomSet.union (follow_rev_x, followSym))
                        else
                            follow_2

                in
                    AtomMap.insert (follow_2, rev_x, AtomSet.union (follow_rev_x, first_suff))
                end
            else
                follow_1


        val first_suff_new = if(AtomSet.member (symbols, rev_x)) then
                let
                    val first_rev_x_opt = AtomMap.find (first, rev_x)
                    val first_rev_x = case first_rev_x_opt of
                        SOME (fst_set) => fst_set
                        | NONE => AtomSet.empty
                in
                    if(AtomSet.member (nullable, rev_x)) then 
                        AtomSet.union (first_suff, first_rev_x)
                    else
                        first_rev_x
                end
                    
            else
                AtomSet.singleton rev_x
        
        val is_nullable_suff_new = if(AtomSet.member (symbols, rev_x) andalso AtomSet.member (nullable, rev_x)) then
                true
            else
                false

    in
        getFollowForProd first follow_new symbols nullable sym first_suff_new is_nullable_suff_new rev_xs
    end


fun getAllForProd nullable first follow symbols sym rhs = let
    
        val nullable_new = getNullableForProd nullable symbols sym rhs
        val first_new = getFirstForProd first symbols nullable_new sym rhs
        val follow_new = getFollowForProd first_new follow symbols nullable_new sym AtomSet.empty true (List.rev rhs)

    in
        (nullable_new, first_new, follow_new)
    end

fun getAllForEachProd nullable first follow symbols sym [] = (nullable, first, follow)
    | getAllForEachProd nullable first follow symbols sym (prod :: prod_list) = let
            val (nullable_new, first_new, follow_new) = getAllForProd nullable first follow symbols sym prod
        in
            getAllForEachProd nullable_new first_new follow_new symbols sym prod_list
        end

exception NonTermHasNoProd

fun getAllForAllNonTerm nullable first follow symbols rules [] = (nullable, first, follow)
    | getAllForAllNonTerm nullable first follow symbols rules (sym :: sym_list) = let

            val prod_setOpt = AtomMap.find (rules, sym)
            val prod_list = case prod_setOpt of
                SOME (prod_set) => RHSSet.listItems prod_set
                | NONE => raise NonTermHasNoProd

            val (nullable_new, first_new, follow_new) = getAllForEachProd nullable first follow symbols sym prod_list
        in
            getAllForAllNonTerm nullable_new first_new follow_new symbols rules sym_list
        end

fun iterateAllUntilFixedPoint nullable first follow symbols rules = let
        
            fun mapEqual (m1, m2) = if(AtomMap.numItems m1 <> AtomMap.numItems m2) then false
                else (
                    let
                        val m1_list = AtomMap.listItemsi m1

                        fun compareListAndMap [] mp = true
                            | compareListAndMap ((key, value) :: ls) mp = let
                                    val value_mp_opt = AtomMap.find (mp, key)
                                in
                                    case value_mp_opt of 
                                        NONE => false
                                        | SOME (value_mp) => if(AtomSet.equal (value, value_mp)) then 
                                                compareListAndMap ls mp
                                            else
                                                false
                                end
                    in
                        compareListAndMap m1_list m2
                    end
                )

            val (nullable_new, first_new, follow_new) = getAllForAllNonTerm nullable first follow symbols rules (AtomSet.listItems symbols)
            val foundFixPoint = if(AtomSet.equal (nullable_new, nullable) andalso mapEqual (first_new, first) andalso mapEqual (follow_new, follow)) then
                    true
                else
                    false
        
        in
            if(foundFixPoint) then 
                (nullable, first, follow)
            else
                iterateAllUntilFixedPoint nullable_new first_new follow_new symbols rules
        end

fun computeAllForGrammer (grammar : Grammar) = iterateAllUntilFixedPoint AtomSet.empty AtomMap.empty AtomMap.empty (#symbols grammar) (#rules grammar)

(* Test cases *)

exception FirstSetNotBuilt
exception FollowSetNotBuilt

fun listStr [] = "\n"
    | listStr (x :: xs) = x ^ " " ^ listStr xs

fun printAll (nullable, first, follow) [] = ()
    | printAll (nullable, first, follow) (sym :: sym_list) = let
        val _ = print ((Atom.toString sym) ^ "\n")

        val _ = if(AtomSet.member (nullable, sym)) then 
                print "Nullable: Yes\n"
            else
                print "Nullable: No\n"

        val first_sym_opt = AtomMap.find (first, sym)
        val first_sym_ls = case first_sym_opt of
            SOME (first_sym) => AtomSet.listItems first_sym
            | NONE => raise FirstSetNotBuilt
        
        val _ = print("First: ")
        val _ = print (listStr (map Atom.toString first_sym_ls))

        val follow_sym_opt = AtomMap.find (follow, sym)
        val follow_sym_ls = case follow_sym_opt of
            SOME (follow_sym) => AtomSet.listItems follow_sym
            | NONE => raise FollowSetNotBuilt
        
        val _ = print("Follow: ")
        val _ = print (listStr (map Atom.toString follow_sym_ls))
    
    in
        printAll (nullable, first, follow) sym_list
    end

fun printAns (grammar : Grammar) = printAll (computeAllForGrammer grammar) (AtomSet.listItems (#symbols grammar))

(* 
    E -> E + id
    E -> id
*)

val s1 = AtomSet.addList (AtomSet.empty, map Atom.atom ["E"])
val t1 = AtomSet.addList (AtomSet.empty, map Atom.atom ["+", "id"])
val r1 = AtomMap.insert (AtomMap.empty, Atom.atom "E", RHSSet.addList (RHSSet.empty, map (map Atom.atom) [["E", "+", "id"], ["id"]]))
val g1 : Grammar = {symbols = s1, tokens = t1, rules = r1}

val _ = printAns g1