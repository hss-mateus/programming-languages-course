type student_id = int
type grade = int
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail { grade = SOME x, id = _ } = if x < 75 then fail else pass
  | pass_or_fail _ = fail

fun has_passed x = if pass_or_fail x = fail then false else true

fun number_passed [] = 0
  | number_passed (x::xs') = if has_passed x
                             then 1 + (number_passed xs')
                             else number_passed xs'

fun number_misgraded [] = 0
  | number_misgraded ((pass_fail, x)::xs') =
    case (pass_fail, has_passed x) of
        (pass, false) => 1 + number_misgraded xs'
      | (fail, true) => 1 + number_misgraded xs'
      | _ => number_misgraded xs'

datatype nat = ZERO | SUCC of nat

exception Negative

fun is_positive ZERO = false
  | is_positive _ = true

fun pred ZERO = raise Negative
  | pred (SUCC x) = x

fun nat_to_int ZERO = 0
  | nat_to_int (SUCC x) = 1 + (nat_to_int x)

fun int_to_nat 0 = ZERO
  | int_to_nat x = SUCC (int_to_nat (x - 1))

fun add (ZERO, x) = x
  | add (x, ZERO) = x
  | add (SUCC x, SUCC y) = SUCC (SUCC (add (x, y)))

fun sub (ZERO, x) = x
  | sub (x, ZERO) = x
  | sub (SUCC x, SUCC y) = sub (x, y)

fun mult (ZERO, _) = ZERO
  | mult (_, ZERO) = ZERO
  | mult (SUCC x, SUCC y) = add (add (x, x), mult (SUCC x, y))

fun less_than (ZERO, ZERO) = false
  | less_than (ZERO, _) = true
  | less_than (_, ZERO) = false
  | less_than (SUCC x, SUCC y) = less_than (x, y)

(* TODO
fun tree_height tree =

fun sum_tree tree =

fun gardener tree =

some functions from
smlfamily.github.io/Basis/list
smlfamily.github.io/Basis/option

fun is_empty set =

fun contains set =

fun to_list set =
*)

