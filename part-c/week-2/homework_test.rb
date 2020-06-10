require './homework.rb'

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

a = Point.new(THREE, FIVE)
unless (a.x == THREE && a.y == FIVE)
  puts 'Point is not initialized properly'
end
unless (a.eval_prog([]) == a)
  puts 'Point eval_prog should return self'
end
unless (a.preprocess_prog == a)
  puts 'Point preprocess_prog should return self'
end
a1 = a.shift(THREE, FIVE)
unless (a1.x == SIX && a1.y == TEN)
  puts 'Point shift not working properly'
end
a2 = a.intersect(Point.new(THREE, FIVE))
unless (a2.x == THREE && a2.y == FIVE)
  puts 'Point intersect not working properly'
end 
a3 = a.intersect(Point.new(FOUR, FIVE))
unless (a3.is_a? NoPoints)
  puts 'Point intersect not working properly'
end

b = Line.new(THREE, FIVE)
unless (b.m == THREE && b.b == FIVE)
  puts 'Line not initialized properly'
end
unless (b.eval_prog([]) == b)
  puts 'Line eval_prog should return self'
end
unless (b.preprocess_prog == b)
  puts 'Line preprocess_prog should return self'
end

b1 = b.shift(THREE, FIVE)
unless (b1.m == THREE && b1.b == ONE)
  puts 'Line shift not working properly'
end

b2 = b.intersect(Line.new(THREE, FIVE))
unless (((b2.is_a? Line)) && b2.m == THREE && b2.b == FIVE)
  puts 'Line intersect not working properly'
end
b3 = b.intersect(Line.new(THREE, FOUR))
unless ((b3.is_a? NoPoints))
  puts 'Line intersect not working properly'
end

c = VerticalLine.new(THREE)
unless (c.x == THREE)
  puts 'VerticalLine not initialized properly'
end

unless (c.eval_prog([]) == c)
  puts 'VerticalLine eval_prog should return self'
end
unless (c.preprocess_prog == c)
  puts 'VerticalLine preprocess_prog should return self'
end
c1 = c.shift(THREE, FIVE)
unless (c1.x == SIX)
  puts 'VerticalLine shift not working properly'
end
c2 = c.intersect(VerticalLine.new(THREE))
unless ((c2.is_a? VerticalLine) && c2.x == THREE )
  puts 'VerticalLine intersect not working properly'
end
c3 = c.intersect(VerticalLine.new(FOUR))
unless ((c3.is_a? NoPoints))
  puts 'VerticalLine intersect not working properly'
end

d = LineSegment.new(ONE, TWO, -THREE, -FOUR)
unless (d.eval_prog([]) == d)
  puts 'LineSegement eval_prog should return self'
end
d1 = LineSegment.new(ONE, TWO, ONE, TWO)
d2 = d1.preprocess_prog
unless ((d2.is_a? Point)&& d2.x == ONE && d2.y == TWO)
  puts 'LineSegment preprocess_prog should convert to a Point'
  puts 'if ends of segment are real_close'
end

d = d.preprocess_prog
unless (d.x1 == -THREE && d.y1 == -FOUR && d.x2 == ONE && d.y2 == TWO)
  puts 'LineSegment preprocess_prog should make x1 && y1'
  puts 'on the left of x2 && y2'
end

d3 = d.shift(THREE, FIVE)
unless (d3.x1 == ZERO && d3.y1 == ONE && d3.x2 == FOUR && d3.y2 == SEVEN)
  puts 'LineSegment shift not working properly'
end

d4 = d.intersect(LineSegment.new(-THREE, -FOUR, ONE, TWO))
unless (((d4.is_a? LineSegment)) && d4.x1 == -THREE && d4.y1 == -FOUR && d4.x2 == ONE && d4.y2 == TWO)
  puts 'LineSegment intersect not working properly'
end

d5 = d.intersect(LineSegment.new(TWO, THREE, FOUR, FIVE))
unless ((d5.is_a? NoPoints))
  puts 'LineSegment intersect not working properly'
end

i = Intersect.new(LineSegment.new(-ONE, -TWO, THREE, FOUR), LineSegment.new(THREE, FOUR, -ONE, -TWO))
i1 = i.preprocess_prog.eval_prog([])
unless (i1.x1 == -ONE && i1.y1 == -TWO && i1.x2 == THREE && i1.y2 == FOUR)
  puts 'Intersect eval_prog should return the intersect between e1 && e2'
end

v = Var.new('a')
v1 = v.eval_prog([['a',  Point.new(THREE, FIVE)]])
unless ((v1.is_a? Point) && v1.x == THREE && v1.y == FIVE)
  puts 'Var eval_prog is not working properly'
end 
unless (v.preprocess_prog == v)
  puts 'Var preprocess_prog should return self'
end

l = Let.new('a', LineSegment.new(-ONE, -TWO, THREE, FOUR),
            Intersect.new(Var.new('a'), LineSegment.new(THREE, FOUR, -ONE, -TWO)))
l1 = l.preprocess_prog.eval_prog([])
unless (l1.x1 == -ONE && l1.y1 == -TWO && l1.x2 == THREE && l1.y2 == FOUR)
  puts 'Let eval_prog should evaluate e2 after adding [s,  e1] to the environment'
end

l2 = Let.new('a', LineSegment.new(-ONE, -TWO, THREE, FOUR),
             Let.new('b', LineSegment.new(THREE, FOUR, -ONE, -TWO), Intersect.new(Var.new('a'), Var.new('b'))))
l2 = l2.preprocess_prog.eval_prog([['a', Point.new(0, 0)]])
unless (l2.x1 == -ONE && l2.y1 == -TWO && l2.x2 == THREE && l2.y2 == FOUR)
  puts 'Let eval_prog should evaluate e2 after adding [s,  e1] to the environment'
end

s = Shift.new(THREE, FIVE, LineSegment.new(-ONE, -TWO, THREE, FOUR))
s1 = s.preprocess_prog.eval_prog([])
unless (s1.x1 == TWO && s1.y1 == THREE && s1.x2 == SIX && s1.y2 == 9)
  puts 'Shift should shift e by dx && dy'
end
