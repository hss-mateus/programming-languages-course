use "extra_exercises.sml";

val test01 = alternate [1, 2, 3, 4] = ~2

val test02 = min_max [1, 2, 3, 4] = (1, 4)

val test03 = partial_sum [1, 4, 20] = [1, 5, 25]

val test04 = greeting (SOME "Fred") = "Hello there, Fred!"

val test05 = greeting NONE = "Hello there, you!"

val test06 = repeat ([1, 2, 3], [4, 0, 3]) = [1, 1, 1, 1, 3, 3, 3]

val test07 = add_opt (SOME 1, SOME 2) = SOME 3

val test08 = add_opt (NONE, SOME 2) = NONE

val test09 = add_all_opt [SOME 1, NONE, SOME 3] = SOME 4

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

val test21 = sorted_merge ([1, 4, 7], [5, 8, 9, 10, 11, 13]) = [1, 4, 5, 7, 8, 9, 10, 11, 13]

val test22 = qsort [11, 13, 1, 42, 2, 11, 3, 2, 14, 8, 7] = [1, 2, 2, 3, 7, 8, 11, 11, 13, 14, 42]

val test23 = divide [1, 2, 3, 4, 5, 6, 7] = ([1, 3, 5, 7], [2, 4, 6])

val test24 = not_so_quick_sort([11, 13 ,1 ,42 ,2 ,11 ,3]) = [1, 2, 3, 11, 11, 13, 42]

val test25 = full_divide (2, 40) = (3, 5)

val test26 = full_divide (3, 10) = (0, 10)
