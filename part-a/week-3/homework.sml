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

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red
  | card_color (Spades, _) = Black

fun card_value (_, Num x) = x
  | card_value (_, Ace) =  11
  | card_value (_, _) = 10

fun remove_card ([], _, exn) = raise exn
  | remove_card ((c::cs'), card, exn) =
    case c = card of
        true => cs'
      | false => c::remove_card (cs', card, exn)

fun all_same_color [] = true
  | all_same_color (_::[]) = true
  | all_same_color (c1::c2::cs') = if (card_color c1) = (card_color c2)
                                   then all_same_color (c1::cs')
                                   else false

fun sum_cards cards =
    let fun aux ([], acc) = acc
          | aux (c::cs', acc) = aux (cs', acc + (card_value c))
    in
        aux (cards, 0)
    end

fun score (cards, goal) =
    let val sum = sum_cards cards
        val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color cards
        then pre_score div 2
        else pre_score
    end

fun officiate (cards, moves, goal) =
    let fun run (cards, helds, []) = score (helds, goal)
          | run ([], helds, (Draw::_)) = score (helds, goal)
          | run ((c::cs'), helds, (Draw::mvs')) = if (sum_cards helds) > goal
                                                  then score (helds, goal)
                                                  else run (cs', (c::helds), mvs')
          | run (cards, helds, ((Discard card)::mvs')) =
            run (cards, (remove_card (helds, card, IllegalMove)), mvs')
    in
        run (cards, [], moves)
    end

fun new_sum_cards cards =
    let fun new_card_value (_, Num x) = x
          | new_card_value (_, Ace) = 1
          | new_card_value (_, _) = 10
        fun aux ([], acc) = acc
          | aux (c::cs', acc) = aux (cs', acc + (new_card_value c))
    in
        aux (cards, 0)
    end

fun score_challenge (cards, goal) =
    let val sum = new_sum_cards cards
        val pre_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
        if all_same_color cards
        then pre_score div 2
        else pre_score
    end

fun officiate_challenge (cards, moves, goal) =
    let fun run (cards, helds, []) = score_challenge (helds, goal)
          | run ([], helds, (Draw::_)) = score_challenge (helds, goal)
          | run ((c::cs'), helds, (Draw::mvs')) = if (new_sum_cards helds) > goal
                                                  then score_challenge (helds, goal)
                                                  else run (cs', (c::helds), mvs')
          | run (cards, helds, ((Discard card)::mvs')) =
            run (cards, (remove_card (helds, card, IllegalMove)), mvs')
        val game1 = officiate (cards, moves, goal)
        val game2 = run (cards, [], moves)
    in
        Int.min (game1, game2)
    end

fun careful_player (cards, goal) =
    let fun aux ([], helds, moves) = moves
          | aux (c::cs', helds, moves) =
            if goal - (sum_cards helds) > 10
            then aux (cs', c::helds, Draw::moves)
            else moves
    in
        aux (cards, [], [])
    end
