use "extra_exercises.sml";

val test1 = alternate [1, 2, 3, 4] = ~2

val test2 = min_max [1, 2, 3, 4] = (1, 4)

val test3 = partial_sum [1, 4, 20] = [1, 5, 25]

val test4 = greeting (SOME "Fred") = "Hello there, Fred!"

val test5 = greeting NONE = "Hello there, you!"

val test6 = repeat ([1, 2, 3], [4, 0, 3]) = [1, 1, 1, 1, 3, 3, 3]

val test7 = add_opt (SOME 1, SOME 2) = SOME 3

val test8 = add_opt (NONE, SOME 2) = NONE

val test9 = add_all_opt [SOME 1, NONE, SOME 3] = SOME 4

val test10 = any [false, false, true, false] = true

val test11 = all [false, false, true, false] = false

val test12 = zip ([1, 2, 3], [4, 6]) = [(1, 4), (2, 6)]

val test13 = zip_recycle ([1, 2, 3], [1, 2, 3, 4, 5, 6, 7])
             = [(1, 1), (2, 2), (3, 3), (1, 4), (2, 5), (3, 6), (1, 7)]

val test14 = zip_opt ([1, 2, 3], [1, 2, 3, 4, 5, 6, 7]) = NONE

val test15 = zip_opt ([1, 2, 3], [1, 2, 3]) = SOME [(1, 1), (2, 2), (3, 3)]

val test16 = lookup ([("foo", 1), ("bar", 2), ("baz", 3)], "bar") = SOME 2

val test17 = splitup [~1, ~2, ~3, 1, 2, 3] = ([~1, ~2, ~3], [1, 2, 3])

val test18 = split_at ([~1, ~2, ~3, 1, 2, 3], ~1) = ([~2, ~3], [~1, 1, 2, 3])

val test19 = is_sorted [1, ~2, 3, 4] = false

val test20 = is_any_sorted [1, 0, ~1, ~2] = true

val test21 = divide [1, 2, 3, 4, 5, 6, 7] = ([1, 3, 5, 7], [2, 4, 6])

val test22 = full_divide (2, 40) = (3, 5)

val test23 = full_divide (3, 10) = (0, 10)
