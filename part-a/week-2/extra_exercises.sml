fun alternate (xs : int list) =
    if null (tl xs)
    then hd xs
    else hd xs - alternate (tl xs)

fun min_max (xs : int list) =
    let
        fun max (xs : int list) =
            if null (tl xs)
            then hd xs
            else if hd xs > hd (tl xs)
            then max (hd xs :: tl (tl xs))
            else max (tl xs)
        fun min (xs : int list) =
            if null (tl xs)
            then hd xs
            else if hd xs < hd (tl xs)
            then min (hd xs :: tl (tl xs))
            else min (tl xs)
    in
        (min xs, max xs)
    end

fun partial_sum (xs : int list) =
    if null (tl xs)
    then hd xs :: []
    else hd xs :: partial_sum (hd xs + hd (tl xs) :: tl (tl xs))

fun greeting (name : string option) =
    let val name = if isSome name then valOf name else "you"
    in
        "Hello there, " ^ name ^ "!"
    end

fun repeat (xs : (int list * int list)) =
    if null (#2 xs)
    then #1 xs
    else if hd (#2 xs) = 0
    then repeat (tl (#1 xs), tl (#2 xs))
    else hd (#1 xs) :: repeat (#1 xs, hd (#2 xs) - 1 :: tl (#2 xs))

fun add_opt (xs : (int option * int option)) =
    if isSome (#1 xs) andalso isSome (#2 xs)
    then SOME ((valOf (#1 xs)) + (valOf (#2 xs)))
    else NONE

fun add_all_opt (xs : int option list) =
    if null xs
    then NONE
    else if null (tl xs)
    then hd xs
    else if not (isSome (hd xs))
    then add_all_opt (tl xs)
    else if not (isSome (hd (tl xs)))
    then add_all_opt (hd xs :: tl (tl xs))
    else add_all_opt (SOME (valOf (hd xs) + valOf (hd (tl xs))) :: tl (tl xs))

fun any (xs : bool list) =
    if null xs
    then false
    else if null (tl xs)
    then hd xs
    else if hd xs
    then true
    else any (tl xs)

fun all (xs : bool list) =
    if null xs
    then true
    else if null (tl xs)
    then hd xs
    else if not (hd xs)
    then false
    else all (tl xs)

fun zip (xs : (int list * int list)) =
    let
        fun make_pairs (xs : (int list * int list), pairs : (int * int) list) =
            if null (#1 xs) orelse null (#2 xs)
            then pairs
            else (hd (#1 xs), hd (#2 xs)) :: make_pairs ((tl (#1 xs), tl (#2 xs)), pairs)
    in
        make_pairs (xs, [])
    end

fun zip_recycle (xs : (int list * int list)) =
    let
        fun make_pairs (xs_copy : (int list * int list), pairs : (int * int) list) =
            if null (#2 xs_copy)
            then pairs
            else if null (#1 xs_copy)
            then make_pairs ((#1 xs, #2 xs_copy), pairs)
            else (hd (#1 xs_copy), hd (#2 xs_copy)) ::
                 make_pairs ((tl (#1 xs_copy), tl (#2 xs_copy)), pairs)
    in
        make_pairs (xs, [])
    end

fun zip_opt (xs : (int list * int list)) =
    let
        fun same_length (xs : (int list * int list)) =
            if null (tl (#1 xs)) andalso not (null (tl (#2 xs)))
               orelse not (null (tl (#1 xs))) andalso null (tl (#2 xs))
            then false
            else if null (tl (#1 xs)) andalso null (tl (#2 xs))
            then true
            else same_length (tl (#1 xs), tl (#2 xs))
    in
        if same_length (#1 xs, #2 xs)
        then SOME (zip xs)
        else NONE
    end

fun lookup (look : ((string * int) list * string)) =
    if #2 look = (#1 (hd (#1 look)))
    then SOME (#2 (hd (#1 look)))
    else if null (tl (#1 look))
    then NONE
    else lookup (tl (#1 look), #2 look)

fun splitup (xs : int list) =
    let
        fun split (xs : int list, lists : (int list * int list)) =
            if null xs
            then lists
            else if hd xs < 0
            then split (tl xs, (#1 lists, (#2 lists) @ [hd xs]))
            else split (tl xs, ((#1 lists) @ [hd xs], #2 lists))
    in
        split (xs, ([], []))
    end

fun split_at (xs : ((int list) * int)) =
    let
        fun split (lst : int list, lists : (int list * int list)) =
            if null lst
            then lists
            else if hd lst <= #2 xs
            then split (tl lst, ((#1 lists) @ [hd lst], #2 lists))
            else split (tl lst, (#1 lists, (#2 lists) @ [hd lst]))
    in
        split (#1 xs, ([], []))
    end

fun is_sorted (xs : int list) =
    if null xs orelse null (tl xs)
    then true
    else if hd xs > hd (tl xs)
    then false
    else is_sorted (tl xs)

fun is_any_sorted (xs : int list) =
    let
        fun is_sorted_dec (xs : int list) =
            if null xs orelse null (tl xs)
            then true
            else if hd xs < hd (tl xs)
            then false
            else is_sorted_dec (tl xs)
    in
        is_sorted xs orelse is_sorted_dec xs
    end

fun sorted_merge (lists : (int list * int list)) =
    let
        fun merge (items : int * int) =
            if #1 items > #2 items
            then [#2 items, #1 items]
            else [#1 items, #2 items]
        fun iterate (lists : (int list * int list), result : int list) =
            if null (#1 lists)
            then result @ (#2 lists)
            else if null (#2 lists)
            then result @ (#1 lists)
            else iterate ((tl (#1 lists), tl (#2 lists)),
                          result @ (merge (hd (#1 lists), hd (#2 lists))))
    in
        iterate(lists, [])
    end

(* TODO
fun qsort (xs : int list) =
 *)

fun divide (xs : int list) =
    let
        fun split (xs : int list, result : (int list * int list)) =
            if null xs
            then result
            else if null (tl xs)
            then split (tl xs, ((#1 result) @ [hd xs], #2 result))
            else split (tl (tl xs), ((#1 result) @ [hd xs], (#2 result) @ [hd (tl xs)]))
    in
        split (xs, ([], []))
    end

fun full_divide (xs : (int * int)) =
    let
        fun divide (attempt : (int * int)) =
            if (#2 attempt) mod (#1 xs) = 0
            then divide ((#1 attempt) + 1, (#2 attempt) div (#1 xs))
            else attempt
    in
        divide (0, #2 xs)
    end
