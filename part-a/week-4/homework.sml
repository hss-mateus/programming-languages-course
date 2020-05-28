fun only_capitals xs = List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (x, acc) => if String.size x > String.size acc
                               then x
                               else acc) "" xs

fun longest_string2 xs =
    List.foldl (fn (x, acc) => if String.size x >= String.size acc
                               then x
                               else acc) "" xs

fun longest_string_helper f xs =
    List.foldl (fn (x, acc) => if f (String.size x, String.size acc)
                               then x
                               else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs') = case f x of
                                  SOME x => x
                                | NONE => first_answer f xs'

fun all_answers f [] = SOME []
  | all_answers f (x::xs') = case (f x, all_answers f xs') of
                                 (NONE, _) => NONE
                               | (_, NONE) => NONE
                               | (SOME x, SOME y) => SOME (x @ y)

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            Wildcard          => f1 ()
          | Variable x        => f2 x
          | TupleP ps         => List.foldl (fn (p, i) => (r p) + i) 0 ps
          | ConstructorP(_,p) => r p
          | _                 => 0
    end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var (str, pat) = g (fn _ => 0) (fn x => if x = str then 1 else 0) pat

fun check_pat pat =
    let fun get_str_list (Variable x) = [x]
          | get_str_list (TupleP ps) = List.foldl (fn (r, acc) => (get_str_list r) @ acc) [] ps
          | get_str_list _ = []
        fun exists x = List.exists (fn y => x = y)
        fun is_unique [] = true
          | is_unique (x::xs') = if exists x xs'
                                 then false
                                 else is_unique xs'
    in
        is_unique (get_str_list pat)
    end

fun match (_, Wildcard) = SOME []
  | match (Const v, ConstP pat) = if v = pat then SOME [] else NONE
  | match (Unit, UnitP) = SOME[]
  | match (Constructor (s1, v), ConstructorP (s2, pat)) = if s1 = s2 then match (v, pat) else NONE
  | match (Tuple xs, TupleP ps) = if List.length xs <> List.length ps
                                  then NONE
                                  else (case all_answers match (ListPair.zip (xs, ps)) of
                                            SOME v => SOME v
                                          | _ => NONE)
  | match (v, Variable str) = SOME [(str, v)]
  | match (_, _) = NONE

fun first_match v pat =
    SOME (first_answer (fn x => match (v, x)) pat)
    handle NoAnswer => NONE

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string
