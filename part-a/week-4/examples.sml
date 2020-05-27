fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times (f, n - 1, x))

fun map (f, []) = []
  | map (f, (x::xs')) =  (f x)::(map (f, xs'))

fun inc xs = map (fn x => x + 1, xs)

fun filter (f, []) = []
  | filter (f, (x::xs')) = if f x
                           then x::(filter (f, xs'))
                           else filter (f, xs')

fun positives xs = filter (fn x => x > 0, xs)

fun double_or_triple f =
    if f 7
    then fn x => x * 2
    else fn x => x * 3

fun double x = (double_or_triple (fn x => true)) x

fun triple x = (double_or_triple (fn x => false)) x

fun fold (f, acc, []) = acc
  | fold (f, acc, (x::xs')) = fold (f, f (acc, x), xs')
