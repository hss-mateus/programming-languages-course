fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, lst) =
    let fun aux [] = []
          | aux (x::xs') = if same_string (x, str)
                           then aux xs'
                           else x::(aux xs')
        val filtered = aux lst
    in
        if filtered = lst then NONE else SOME filtered
    end

fun get_substitutions1 ([], str) = []
  | get_substitutions1 ((x::xs'), str) =
    case (all_except_option (str, x)) of
        NONE => get_substitutions1 (xs', str)
      | SOME x => x @ get_substitutions1 (xs', str)

fun get_substitutions2 (lsts, str) =
    let fun aux ([], acc) = acc
          | aux ((x::xs'), acc) =
            case (all_except_option (str, x)) of
                NONE => aux (xs', acc)
              | SOME x => aux (xs', acc @ x)
    in
        aux (lsts, [])
    end

fun similar_names (lsts, { first, middle, last }) =
    let val firsts = get_substitutions2 (lsts, first)
        fun aux [] = []
          | aux (x::xs') = { first = x, middle = middle, last = last }::(aux xs')
    in
        aux (first::firsts)
    end

(*
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove
*)
