fun alternate [] = 0
  | alternate (x::[]) = x
  | alternate (x::xs) = x - (alternate xs)

fun min_max xs =
    let fun aux (x, (min, max)) =
            (Int.min (x, min), (Int.max (x, max)))
    in
        List.foldl aux (hd xs, hd xs) xs
    end

fun partial_sum [] = []
  | partial_sum (x::[]) = x::[]
  | partial_sum (x::y::xs) = x::partial_sum ((x + y)::xs)

fun greeting name = "Hello there, " ^ getOpt (name, "you") ^ "!"

fun repeat ([], _) = []
  | repeat (xs, []) = xs
  | repeat (x::xs, 0::counts) = repeat (xs, counts)
  | repeat (x::xs, c::counts) = x::(repeat (x::xs, (c - 1)::counts))

fun add_opt (SOME x, SOME y) = SOME (x + y)
  | add_opt _ = NONE

fun add_all_opt [] = NONE
  | add_all_opt (x::[]) = x
  | add_all_opt (NONE::xs) = add_all_opt xs
  | add_all_opt (x::NONE::xs) = add_all_opt (x::xs)
  | add_all_opt ((SOME x)::(SOME y)::xs) = add_all_opt ((SOME (x + y))::xs)

fun any [] = false
  | any (x::[]) = x
  | any (x::xs) = x orelse any xs

fun all [] = true
  | all (x::[]) = x
  | all (x::xs) = x andalso all xs

fun zip  ([], _) = []
  | zip (_, []) = []
  | zip (x::xs, y::ys) = (x, y)::zip (xs, ys)

fun zip_recycle (xs, ys) =
    let fun aux ([], []) = []
          | aux ([], ys_copy) = aux (xs, ys_copy)
          | aux (xs_copy, []) = []
          | aux (x::xs, y::ys) = (x, y)::aux (xs, ys)
    in
        aux (xs, ys)
    end

fun zip_opt (xs, ys) =
    if length xs = length ys
    then SOME (zip (xs, ys))
    else NONE

fun lookup ([], _) = NONE
  | lookup ((s, i)::xs, str) = if s = str
                               then SOME i
                               else lookup (xs, str)

fun splitup xs =
    let fun aux ([], lsts) = lsts
          | aux (x::xs, (neg, pos)) = if x < 0
                                      then aux (xs, (neg @ [x], pos))
                                      else aux (xs, (neg, pos @ [x]))
    in
        aux (xs, ([], []))
    end

fun split_at (xs, n) =
    let fun aux ([], lsts) = lsts
          | aux (x::xs, (l, r)) = if x < n
                                   then aux (xs, (l @ [x], r))
                                   else aux (xs, (l, r @ [x]))
    in
        aux (xs, ([], []))
    end

fun is_sorted [] = true
  | is_sorted (x::[]) = true
  | is_sorted (x::y::xs) = x < y andalso is_sorted (y::xs)

fun is_any_sorted xs =
    let fun is_sorted_dec [] = true
          | is_sorted_dec (x::[]) = true
          | is_sorted_dec (x::y::xs) = x > y andalso is_sorted_dec (y::xs)
    in
        is_sorted xs orelse is_sorted_dec xs
    end

fun sorted_merge ([], []) = []
  | sorted_merge ([], ys) = ys
  | sorted_merge (xs, []) = xs
  | sorted_merge (x::xs, y::ys) = if x > y
                                  then y::(sorted_merge (x::xs, ys))
                                  else x::(sorted_merge (xs, y::ys))

fun qsort [] = []
  | qsort (x::xs) =
    let val (left, right) = split_at (xs, x)
    in
        (qsort left) @ x :: (qsort right)
    end

fun divide xs =
    let fun aux ([], lsts) = lsts
          | aux (x::[], (l, r)) = aux ([], (l @ [x], r))
          | aux (x1::x2::xs, (l, r)) = aux (xs, (l @ [x1], r @ [x2]))
    in
        aux (xs, ([], []))
    end

fun not_so_quick_sort [] = []
  | not_so_quick_sort xs =
    let
        val (xs, ys) = divide xs
    in
        sorted_merge (qsort xs, qsort ys)
    end

fun full_divide (k, n) =
    let fun aux (d, n2) = if n2 mod k = 0
                          then aux (d + 1, n2 div k)
                          else (d, n2)
    in
        aux (0, n)
    end

(* TODO
fun factorize x =

fun multiply pairs =

fun all_products pairs =
*)
