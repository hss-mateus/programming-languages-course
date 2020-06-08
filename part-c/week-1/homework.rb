class MyPiece < Piece
  All_My_Pieces = All_Pieces +
                  [rotations([[0, 0], [0, -1], [1, 0], [-1, 0], [-1, -1]]),
                   rotations([[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]]),
                   rotations([[0, 0], [-1, 0], [-1, -1]])]

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat = false
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position

    locations.length.times do |i|
      current = locations[i]

      x = current[1] + displacement[1]
      y = current[0] + displacement[0]

      @grid[x][y] = @current_pos[i]
    end

    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.new([[0, 0]], self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end

    @current_pos = nil
  end

  def rotate_180
    @current_block.move(0, 0, 2) unless game_over? && @game.is_running?
    draw
  end

  def cheat
    return unless @score >= 100 && !@cheat

    @cheat = true
    @score -= 100
  end
end

class MyTetris < Tetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super

    @root.bind('u', proc { @board.rotate_180 })
    @root.bind('c', proc { @board.cheat })
  end
end
