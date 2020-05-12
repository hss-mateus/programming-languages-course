fun list_sum (xs : int list) =
    if null xs
    then 0
    else hd xs + list_sum (tl xs)

fun list_product (xs : int list) =
    if null xs
    then 1
    else hd xs * list_product (tl xs)

fun countdown (x : int) =
    if x = 0
    then []
    else x :: countdown(x - 1)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else (hd xs) :: append (tl xs, ys)

fun pair_sum (pair : (int * int)) =
    #1 pair + #2 pair

fun pair_list_sum (xs : (int * int) list) =
    if null xs
    then 0
    else pair_sum (hd xs) + pair_list_sum (tl xs)

fun firsts (xs : (int * int) list) =
    if null xs
    then []
    else (#1 (hd xs)) :: firsts (tl xs)

fun seconds (xs : (int * int) list) =
    if null xs
    then []
    else (#2 (hd xs)) :: seconds (tl xs)

fun pair_list_sum2 (xs : (int * int) list) =
    (list_sum (firsts xs)) + (list_sum (seconds xs))

fun factorial (n : int) =
    list_product (countdown n)

fun maximum (xs : int list) =
    if null xs
    then NONE
    else
        let fun max_nonempty (xs : int list) =
                if null (tl xs)
                then hd xs
                else let val tl_ans = max_nonempty (tl xs)
                     in
                         if hd xs > tl_ans
                         then hd xs
                         else tl_ans
                     end
        in
            SOME (max_nonempty xs)
        end
