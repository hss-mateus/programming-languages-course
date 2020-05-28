use "homework.sml";

(* Provided tests *)
val test01 = only_capitals ["A", "B", "C"] = ["A", "B", "C"]

val test02 = longest_string1 ["A", "bc", "C"] = "bc"

val test03 = longest_string2 ["A", "bc", "C"] = "bc"

val test04 = longest_string3 ["A", "bc", "C"] = "bc"

val test05 = longest_string4 ["A", "B", "C"] = "C"

val test06 = longest_capitalized ["A", "bc", "C"] = "A"

val test07 = rev_string "abc" = "cba"

val test08 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4, 5] = 4

val test09 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2, 3, 4, 5, 6, 7] = NONE

val test10 = count_wildcards Wildcard = 1

val test11 = count_wild_and_variable_lengths (Variable("a")) = 1

val test12 = count_some_var ("x", Variable("x")) = 1

val test13 = check_pat (Variable("x")) = true

val test14 = match (Const(1), UnitP) = NONE

val test15 = first_match Unit [UnitP] = SOME []

(* Community tests *)
val test16 = only_capitals ["a", "B", "C"] = ["B", "C"]

val test17 = only_capitals ["Abc", "ABc", "abC"] = ["Abc", "ABc"]

val test18 = only_capitals ["1AB", "?AB", "Abc", "ABc", "abC"] = ["Abc", "ABc"]

val test19 = longest_string1 ["A", "bc", "C", "de"] = "bc"

val test20 = longest_string1 ["A", "bc", "C", "def"] = "def"

val test21 = longest_string2 ["A", "bc", "C", "de"] = "de"

val test22 = longest_string2 ["A", "bc", "C", "def"] = "def"

val test23 = longest_string3 ["A", "bc", "C", "de"] = "bc"

val test24 = longest_string3 ["A", "bc", "C", "def"] = "def"

val test25 = longest_string4 ["A", "bc", "C", "de"] = "de"

val test26 = longest_string4 ["A", "bc", "C", "def"] = "def"

val test27 = longest_capitalized [] = ""

val test28 = longest_capitalized ["ab", "a", "b"] = ""

val test29 = rev_string "" = ""

val test30 = first_answer (fn x => if x > 3 then SOME x else NONE) [4, 2, 3, 5] = 4

val test31 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3] ; false) handle NoAnswer => true

val test32 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3] ; false) handle OtherException => true

val test33 = first_answer (fn x => if x > 3 then SOME x else NONE) [1, 2, 3, 4, 2] = 4

val test34 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [3, 2, 4, 5, 6, 7] = NONE

val test35 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2, 4, 5, 6, 8] = NONE

val test36 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2, 4, 6, 8] = SOME [2, 4, 6, 8]

val test37 = all_answers (fn x => if x mod 2 = 0 then SOME [x, x + 1] else NONE) [2, 4, 6, 8] = SOME [2, 3, 4, 5, 6, 7, 8, 9]

val test38 = all_answers (fn x => if x mod 2 = 0 then SOME [] else NONE) [2, 4, 6, 8] = SOME []

val test39 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [] = SOME []

val test41 = count_wildcards (Variable "str") = 0

val test41 = count_wildcards (TupleP [Wildcard, ConstP 12, Wildcard]) = 2

val test42 = count_wildcards (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2

val test43 = count_wild_and_variable_lengths Wildcard = 1

val test44 = count_wild_and_variable_lengths (TupleP [Wildcard, ConstP 12, Wildcard]) = 2

val test45 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard]) = 5

val test46 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard, Variable "str2"]) = 9

val test47 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2

val test48 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, Variable "str", Wildcard]))) = 5

val test49 = count_some_var ("x", (TupleP [Wildcard, ConstP 12, Wildcard])) = 0

val test50 = count_some_var ("x", (TupleP [Wildcard, Variable "str", Wildcard])) = 0

val test51 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard])) = 1

val test52 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"])) = 2

val test53 = count_some_var ("x", (ConstructorP("pattern", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1

val test54 = count_some_var ("x", (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1

val test55 = check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true

val test56 = check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true

val test57 = check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false

val test58 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true

val test59 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true

val test60 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false

val test61 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true

val test62 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true

val test63 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false

val test64 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true

val test65 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false

val test66 = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true

val test67 = match (Const(1), ConstP 1) = SOME []

val test68 = match (Const(1), Variable "s") = SOME [("s", Const(1))]

val test69 = match (Const(1), TupleP [Wildcard]) = NONE

val test70 = match (Const(1), TupleP [ConstP 1]) = NONE

val test71 = match (Tuple [Unit], TupleP [UnitP]) = SOME []

val test72 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []

val test73 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE

val test74 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []

val test75 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]

val test76 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE

val test77 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE

val test78 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]

val test79 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]

val test80 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE
