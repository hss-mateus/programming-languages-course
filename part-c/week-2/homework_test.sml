use "homework.sml";

fun real_equal(x,y) = Real.compare(x,y) = General.EQUAL;

val test1 = let
    val Point(a, b) = preprocess_prog(LineSegment(3.2, 4.1, 3.2, 4.1))
    val Point(c, d) = Point(3.2, 4.1)
in
    real_equal(a, c) andalso real_equal(b, d)
end

val test2 = let
    val LineSegment(a, b, c, d) = preprocess_prog (LineSegment(3.2, 4.1, ~3.2, ~4.1))
    val LineSegment(e, f, g, h) = LineSegment(~3.2, ~4.1, 3.2, 4.1)
in
    real_equal(a, e) andalso real_equal(b, f) andalso real_equal(c, g) andalso real_equal(d, h)
end

val test3 = let
    val Point(a, b) = (eval_prog (preprocess_prog (Shift(3.0, 4.0, Point(4.0, 4.0))), []))
    val Point(c, d) = Point(7.0, 8.0)
in
    real_equal(a, c) andalso real_equal(b, d)
end

val test4 = let
    val Point(a, b) = (eval_prog (Shift(3.0, 4.0, Var "a"), [("a", Point(4.0, 4.0))]))
    val Point(c, d) = Point(7.0, 8.0)
in
    real_equal(a, c) andalso real_equal(b, d)
end

val test5 = let
    val Point(a, b) = (eval_prog (Shift(3.0, 4.0, Var "a"), [("a", Point(4.0, 4.0)),("a", Point(1.0, 1.0))]))
    val Point(c, d) = Point(7.0, 8.0)
in
    real_equal(a, c) andalso real_equal(b, d)
end
