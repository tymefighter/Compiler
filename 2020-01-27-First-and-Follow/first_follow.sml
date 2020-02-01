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

            val firstxOpt = AtomMap.find (first_1, x)
            val (first_2, firstx) = (
                case firstxOpt of
                    NONE => (AtomMap.insert (first_1, x, AtomSet.empty), AtomSet.empty) (* first does not contain x, so insert it *)
                    | SOME (first_x) => (first_1, first_x)                              (* first contains x, so return its first set *)
            )

            val first_new = AtomMap.insert (first_2, sym, AtomSet.union (firstSym, firstx))

        in
            if(AtomSet.member (symbols, x) andalso AtomSet.member (nullable, x)) then
                getFirstForProd first_new symbols nullable sym xs
            else
                first_new
        end

(* Imp: The RHS list has been reversed and passed here for efficient computation of first and nullable of suffixes of actual RHS list *)
(* fun getFollowForProd follow symbols nullable sym first_suff is_nullable_suff [] = follow 
    | getFollowForProd follow symbols nullable sym first_suff is_nullable_suff (rev_x :: rev_xs) = let

        val followSymOpt = AtomMap.find (follow, sym)
        val (follow_1, followSym) = (
            case followSymOpt of 
                NONE => (AtomMap.insert (follow, sym, AtomSet.empty), AtomSet.empty)   (* follow does not contain sym, so insert it *)
                | SOME (follow_sym) => (follow, follow_sym)                            (* follow contains sym, so return its follow set *)
        )

        val follow_rev_xOp = AtomMap.find (follow_1, rev_x)
        val (follow_2, follow_rev_x) = (
            case follow_rev_xOp of
                NONE => (AtomMap.insert (follow_1, rev_x, AtomSet.empty), AtomSet.empty)  (* follow does not contain rev_x, so insert it *)
                | SOME (follow_rev_x_found) => (follow_1, follow_rev_x_found)             (* follow contains rev_x, so return its follow set *)
        ) *)


