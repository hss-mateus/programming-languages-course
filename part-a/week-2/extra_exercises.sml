fun alternate (x::[]) = x
  | alternate (x::xs') = x - (alternate xs')

fun min_max xs =
    let fun min (x::[]) = x
          | min (x1::x2::xs') = if x1 < x2 then min (x1::xs') else min (x2::xs')
        fun max (x::[]) = x
          | max (x1::x2::xs') = if x1 > x2 then max (x1::xs') else max (x2::xs')
    in
        (min xs, max xs)
    end

fun partial_sum (x::[]) = x::[]
  | partial_sum (x1::x2::xs') = x1::partial_sum ((x1 + x2)::xs')

fun greeting (SOME name) = "Hello there, " ^ name ^ "!"
  | greeting NONE = "Hello there, you!"

fun repeat ([], _) = []
  | repeat (x::xs', 0::counts) = repeat (xs', counts)
  | repeat (x::xs', c::counts) = x::(repeat (x::xs', (c - 1)::counts))

fun add_opt (SOME x, SOME y) = SOME (x + y)
  | add_opt _ = NONE

fun add_all_opt [] = NONE
  | add_all_opt (x::[]) = x
  | add_all_opt (NONE::xs') = add_all_opt xs'
  | add_all_opt (x::NONE::xs') = add_all_opt (x::xs')
  | add_all_opt ((SOME x1)::(SOME x2)::xs') = add_all_opt ((SOME (x1 + x2))::xs')

fun any [] = false
  | any (x::[]) = x
  | any (x::xs') = x orelse any xs'

fun all [] = true
  | all (x::[]) = x
  | all (x::xs') = if not x then false else all xs'

fun zip  ([], _) = []
  | zip (_, []) = []
  | zip (x::xs', y::ys') = (x, y)::zip (xs', ys')

fun zip_recycle (xs, ys) =
    let fun aux ([], []) = []
          | aux ([], ys_copy) = aux (xs, ys_copy)
          | aux (xs_copy, []) = []
          | aux (x::xs', y::ys') = (x, y)::(aux (xs', ys'))
    in
        aux (xs, ys)
    end

fun zip_opt xs =
    let fun same_length ([], _::_) = false
          | same_length (_::_, []) = false
          | same_length ([], []) = true
          | same_length (_::xs', _::ys') = same_length (xs', ys')
    in
        if same_length xs
        then SOME (zip xs)
        else NONE
    end

fun lookup ([], _) = NONE
  | lookup ((s, i)::xs', str) = if s = str
                                then SOME i
                                else lookup (xs', str)

fun splitup xs =
    let fun aux ([], lsts) = lsts
          | aux (x::xs', (neg, pos)) = if x < 0
                                       then aux (xs', (neg @ [x], pos))
                                       else aux (xs', (neg, pos @ [x]))
    in
        aux (xs, ([], []))
    end

fun split_at (xs, n) =
    let fun aux ([], lsts) = lsts
          | aux (x::xs', (l, r)) = if x < n
                                   then aux (xs', (l @ [x], r))
                                   else aux (xs', (l, r @ [x]))
    in
        aux (xs, ([], []))
    end

fun is_sorted [] = true
  | is_sorted (x::[]) = true
  | is_sorted (x1::x2::xs') = if x1 < x2
                              then is_sorted (x2::xs')
                              else false

fun is_any_sorted xs =
    let fun is_sorted_dec [] = true
          | is_sorted_dec (x::[]) = true
          | is_sorted_dec (x1::x2::xs') = if x1 > x2
                                          then is_sorted_dec (x2::xs')
                                          else false
    in
        is_sorted xs orelse is_sorted_dec xs
    end

fun sorted_merge lsts =
    let fun aux ([], []) = []
          | aux (x::xs', y::ys') = if x > y
                                   then [y, x]::aux (xs', ys')
                                   else [x, y]::aux (xs', ys')
    in
        aux lsts
    end

fun divide xs =
    let fun aux ([], lsts) = lsts
          | aux (x::[], (l, r)) = aux ([], (l @ [x], r))
          | aux (x1::x2::xs', (l, r)) = aux (xs', (l @ [x1], r @ [x2]))
    in
        aux (xs, ([], []))
    end

fun full_divide (k, n) =
    let fun aux (d, n2) = if n2 mod k = 0
                          then aux (d + 1, n2 div k)
                          else (d, n2)
    in
        aux (0, n)
    end

(* TODO
fun qstort xs =

fun not_so_quick_sort xs =

fun factorize x =

fun multiply pairs =

fun all_products pairs =
*)
