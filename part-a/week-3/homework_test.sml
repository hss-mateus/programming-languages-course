use "homework.sml";

(* Provided tests *)
val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"], ["Elizabeth","Betty"], ["Freddie","Fred","F"]],
                           { first = "Fred", middle = "W", last = "Smith" })
            = [{ first = "Fred",     last = "Smith", middle = "W" },
               { first = "Fredrick", last = "Smith", middle = "W" },
               { first = "Freddie",  last = "Smith", middle = "W" },
               { first = "F",        last = "Smith", middle = "W" }]


val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2), (Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2), (Clubs, Num 4)], 10) = 4

val test11 = officiate ([(Hearts, Num 2), (Clubs, Num 4)], [Draw], 15) = 6

val test12 = officiate ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        42) = 3

val test13 = ((officiate ([(Clubs, Jack), (Spades, Num(8))],
                          [Draw, Discard(Hearts, Jack)],
                          42);
               false)
              handle IllegalMove => true)

(* Personal tests *)
val test14 = all_except_option ("exclude", ["foo", "exclude", "bar", "exclude", "baz", "exclude"])
    = SOME ["foo", "bar", "baz"]

val test15 = get_substitutions1 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred")
    = ["Fredrick", "Freddie", "F"]

val test16 = get_substitutions1 ([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
    = ["Jeffrey", "Geoff", "Jeffrey"]

val test17 = get_substitutions2 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred")
    = ["Fredrick", "Freddie", "F"]

val test18 = get_substitutions2 ([["Fred", "Fredrick"], ["Jeff", "Jeffrey"], ["Geoff", "Jeff", "Jeffrey"]], "Jeff")
    = ["Jeffrey", "Geoff", "Jeffrey"]

val test19 = card_value (Clubs, Ace) = 11
val test20 = card_value (Clubs, Jack) = 10

val test21 = remove_card ([(Clubs, Ace), (Clubs, Jack), (Hearts, Num 7)], (Clubs, Jack), IllegalMove)
    = [(Clubs, Ace), (Hearts, Num 7)]

val test22 = all_same_color [(Hearts, Jack), (Spades, Jack), (Hearts, Ace)] = false

val test24 = score ([(Hearts, Num 2), (Hearts, Num 4)], 10) = 2
val test25 = score ([(Hearts, Num 6), (Hearts, Num 7)], 10) = 4

val test26 = officiate_challenge ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                                  [Draw, Draw, Draw, Draw, Draw],
                                  42) = 3

val test27 = officiate_challenge ([(Hearts, Ace), (Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                                  [Draw, Draw, Draw, Draw, Draw, Draw],
                                  42) = 6

val test28 = officiate_challenge ([(Clubs, Ace), (Clubs, King), (Spades, Queen), (Clubs,King), (Hearts, Queen)],
                                  [Draw, Draw, Draw, Draw, Draw, Draw],
                                  42) = 1

val test29 = officiate_challenge ([(Clubs, Ace), (Spades, Queen), (Clubs, King), (Hearts, Queen)],
                                  [Draw, Draw, Draw, Draw, Draw, Draw],
                                  42)= 1

val test30 = officiate_challenge ([(Clubs, Ace), (Spades, Queen), (Clubs, King), (Hearts, Queen)],
                                  [Draw, Draw, Draw, Draw, Draw, Draw],
                                  40) = 3

val test31 = officiate ([(Spades, Num 7), (Hearts, King), (Clubs, Ace), (Diamonds, Num 2)],
                        careful_player ([(Spades, Num 7),(Hearts, King),(Clubs, Ace),(Diamonds, Num 2)], 18),
                        18) = 1
