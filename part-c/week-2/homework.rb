class GeometryExpression
  Epsilon = 0.00001
end

class GeometryValue
  private

  def real_close(r1, r2)
    (r1 - r2).abs < GeometryExpression::Epsilon
  end

  def real_close_point(x1, y1, x2, y2)
    real_close(x1, x2) && real_close(y1, y2)
  end

  def two_points_to_line(x1, y1, x2, y2)
    if real_close(x1, x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m, b)
    end
  end

  public

  def intersectNoPoints(np)
    np
  end

  def intersectLineSegment(seg)
    line_result = intersect(two_points_to_line(seg.x1, seg.y1, seg.x2, seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    self
  end

  def intersect(other)
    other.intersectNoPoints self
  end

  def intersectPoint(p)
    self
  end

  def intersectLine(line)
    self
  end

  def intersectVerticalLine(vline)
    self
  end

  def intersectWithSegmentAsLineResult(seg)
    self
  end
end

class Point < GeometryValue
  attr_reader :x, :y
  def initialize(x, y)
    @x = x
    @y = y
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(a, b)
    Point.new(x + a, y + b)
  end

  def intersect(other)
    other.intersectPoint self
  end

  def intersectPoint(point)
    if real_close_point(x, y, point.x, point.y)
      self
    else
      NoPoints.new
    end
  end

  def intersectLine(line)
    if real_close(y, line.m * x + line.b)
      self
    else
      NoPoints.new
    end
  end

  def intersectVerticalLine(line)
    if real_close(x, line.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult(segment)
    def inBetween(v, end1, end2)
      epsilon = GeometryExpression::Epsilon
      ((end1 - epsilon <= v and v <= end2 + epsilon) or
       (end2 - epsilon <= v and v <= end1 + epsilon))
    end

    if inBetween(x, segment.x1, segment.x2) && inBetween(y, segment.y1, segment.y2)
      self
    else
      NoPoints
    end
  end
end

class Line < GeometryValue
  attr_reader :m, :b

  def initialize(m, b)
    @m = m
    @b = b
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    Line.new(m, b + dy - m * dx)
  end

  def intersect(other)
    other.intersectLine self
  end

  def intersectPoint(point)
    point.intersectLine self
  end

  def intersectLine(line)
    if real_close(m, line.m)
      if real_close(b, line.b)
        self
      else
        NoPoints.new
      end
    else
      intersect_x = (line.b - b) / (m - line.m)
      intersect_y = m * intersect_x + b

      Point.new(intersect_x, intersect_y)
    end
  end

  def intersectVerticalLine(line)
    Point.new(line.x, m * line.x + b)
  end

  def intersectWithSegmentAsLineResult(segment)
    self
  end
end

class VerticalLine < GeometryValue
  attr_reader :x

  def initialize x
    @x = x
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    VerticalLine.new(x + dx)
  end

  def intersect(other)
    other.intersectVerticalLine self
  end

  def intersectPoint(point)
    point.intersectVerticalLine self
  end

  def intersectLine(line)
    line.intersectVerticalLine self
  end

  def intersectVerticalLine(line)
    if real_close(x, line.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult(segment)
    self
  end
end

class LineSegment < GeometryValue
  attr_reader :x1, :y1, :x2, :y2

  def initialize (x1, y1, x2, y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    if real_close_point(x1, y1, x2, y2)
      Point.new(x1, y1)
    elsif real_close(x1, x2)
      if y1 < y2
        self
      else
        LineSegment.new(x2, y2, x1, y1)
      end
    elsif x1 < x2
      self
    else
      LineSegment.new(x2, y2, x1, y1)
    end
  end

  def shift(dx, dy)
    LineSegment.new(x1 + dx, y1 + dy, x2 + dx, y2 + dy)
  end

  def intersect(other)
    other.intersectLineSegment self
  end

  def intersectPoint(point)
    point.intersectLineSegment self
  end

  def intersectLine(line)
    line.intersectLineSegment self
  end

  def intersectVerticalLine(line)
    line.intersectLineSegment self
  end

  def intersectWithSegmentAsLineResult(seg)
    (x1start, y1start, x1end, y1end) = x1, y1, x2, y2
    (x2start, y2start, x2end, y2end) = seg.x1, seg.y1, seg.x2, seg.y2

    if real_close(x1start, x1end)
      if y1start < y2start
        (aXstart, aYstart, aXend, aYend, bXstart, bYstart, bXend, bYend) =
          x1, y1, x2, y2, seg.x1, seg.y1, seg.x2, seg.y2
      else
        (aXstart, aYstart, aXend, aYend, bXstart, bYstart, bXend, bYend) =
          seg.x1, seg.y1, seg.x2, seg.y2, x1, y1, x2, y2
      end

      if real_close(aYend, bYend)
        Point.new(aXend, aYend)
      elsif aYend < bYstart
        NoPoints.new
      elsif aYend > bYend
        LineSegment.new(bXstart, bYstart, bXend, bYend)
      else
        LineSegment.new(bXstart, bYstart, aXend, aYend)
      end
    else
      if x1start < x2start
        (aXstart, aYstart, aXend, aYend, bXstart, bYstart, bXend, bYend) =
          x1, y1, x2, y2, seg.x1, seg.y1, seg.x2, seg.y2
      else
        (aXstart, aYstart, aXend, aYend, bXstart, bYstart, bXend, bYend) =
          seg.x1, seg.y1, seg.x2, seg.y2, x1, y1, x2, y2
      end

      if real_close(aXend, bXstart)
        Point.new(aXend, aYend)
      elsif aXend < bXstart
        NoPoints.new
      elsif aXend > bXend
        LineSegment.new(bXstart, bYstart, bXend, bYend)
      else
        LineSegment.new(bXstart, bYstart, aXend, aYend)
      end
    end
  end
end

class Intersect < GeometryExpression
  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    pe1 = @e1.preprocess_prog
    pe2 = @e2.preprocess_prog
    Intersect.new(pe1, pe2)
  end

  def eval_prog(env)
    Intersect.new(@e1.eval_prog(env), @e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  def initialize(s, e1, e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    pe1 = @e1.preprocess_prog
    pe2 = @e2.preprocess_prog
    Let.new(s, pe1, pe2)
  end

  def eval_prog(env)
    ext_env = Array.new(env.length) { |i| Array.new(env[i]) }.push(s, e1.eval_prog(env))

    e2.eval_prog(ext_env)
  end
end

class Var < GeometryExpression
  def initialize(s)
    @s = s
  end

  def eval_prog(env)
    pr = env.assoc @s
    raise 'undefined variable' if pr.nil?

    pr[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  def initialize(dx, dy, e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    pe = @e.preprocess_prog
    Shift.new(dx, dy, pe)
  end

  def eval_prog(env)
    e.eval_prog(env).shift(dx, dy)
  end
end
