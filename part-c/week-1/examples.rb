class MyRational
  def initialize(num, den = 1)
    raise 'MyRational received an inappropriate argument' if den.zero?

    if den.negative?
      @num = -num
      @den = -den
    else
      @num = num
      @den = den
    end

    reduce
  end

  def to_s
    ans = @num.to_s

    if @den != 1
      ans += '/'
      ans += @den.to_s
    end

    ans
  end

  def add!(rational)
    a = rational.num
    b = rational.den
    c = @num
    d = @den

    @num = (a * d) + (b * c)
    @den = b * d

    reduce

    self
  end

  def +(other)
    ans = MyRational.new(@num, @den)
    ans.add! other
  end

  protected

  attr_reader :num, :den

  private

  def gcd(first, second)
    if first == second
      first
    elsif first < second
      gcd(first, second - first)
    else
      gcd(second, first)
    end
  end

  def reduce
    if @num.zero?
      @den = 1
    else
      d = gcd(@num.abs, @den)
      @num /= d
      @den /= d
    end
  end
end
