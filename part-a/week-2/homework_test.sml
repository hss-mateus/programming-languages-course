use "homework.sml";

val test1 = is_older ((1, 2, 3), (2, 3, 4)) = true

val test2 = number_in_month ([(2012, 2, 28), (2013, 12, 1)], 2) = 1

val test3 = number_in_months ([(2012, 2, 28), (2013, 12, 1), (2011, 3, 31), (2011, 4, 28)], [2, 3, 4]) = 3

val test4 = dates_in_month ([(2012, 2, 28), (2013, 12, 1)], 2) = [(2012, 2, 28)]

val test5 = dates_in_months ([(2012, 2, 28), (2013, 12, 1), (2011, 3, 31), (2011, 4, 28)], [2, 3, 4]) = [(2012, 2, 28), (2011, 3, 31), (2011, 4, 28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1, 2, 3, 4, 5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1, 2, 2, 2]

val test11 = oldest ([(2012, 2, 28), (2011, 3, 31), (2011, 4, 28)]) = SOME (2011, 3, 31)

val test12 = number_in_months_challenge ([(2012, 2, 28), (2013, 12, 1), (2011, 3, 31), (2011, 4, 28)], [2, 3, 4, 4]) = 3

val test13 = reasonable_date (2019, 2, 31) = false

(* Failed grader tests *)
val test14 = oldest [(5, 5, 2), (5, 10, 2), (5, 2, 2), (5, 12, 2)] = SOME (5, 2, 2)

val test15 = oldest [] = NONE

val test16 = reasonable_date (2004, 2, 29) = true

val test17 = number_in_months ([(1, 12, 29), (3, 2, 28), (1, 2, 27),(1, 2, 25), (6, 7, 8)], []) = 0

val test18 = dates_in_months ([(1, 12, 29), (3, 2, 28), (1, 2, 27), (1, 2, 25), (6, 7, 8)], []) = []

val test19 = reasonable_date (2013, 0, 10) = false

val test20 = reasonable_date (2013, 13, 10) = false
