use "extra_exercises.sml";

val test1 = pass_or_fail { grade = SOME 60, id = 20 } = fail

val test2 = has_passed { grade = SOME 90, id = 30 } = true

val test3 = number_passed [{ grade = NONE, id = 10 },
                           { grade = SOME 90, id = 20},
                           { grade = SOME 60, id = 30 },
                           { grade = SOME 100, id = 40 }] = 2

val test4 = number_misgraded [(pass, { grade = SOME 60, id = 10 }),
                              (fail, { grade = SOME 60, id = 20 }),
                              (fail, { grade = SOME 90, id = 30 })] = 2

val test5 = is_positive ZERO = false

val test6 = is_positive (SUCC (SUCC ZERO)) = true

val test7 = pred (SUCC (SUCC ZERO)) = SUCC ZERO

val test8 = nat_to_int (SUCC (SUCC ZERO)) = 2

val test9 = int_to_nat 2 = (SUCC (SUCC ZERO))

val test10 = add (SUCC (SUCC ZERO), SUCC (SUCC (SUCC ZERO)))
             = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))

val test11 = add (ZERO, ZERO) = ZERO

val test12 = sub (SUCC (SUCC ZERO), SUCC ZERO) = SUCC ZERO

val test13 = mult (SUCC (SUCC ZERO), SUCC (SUCC ZERO))
             = SUCC (SUCC (SUCC (SUCC ZERO)))

val test14 = less_than (SUCC (SUCC ZERO), SUCC ZERO) = false
