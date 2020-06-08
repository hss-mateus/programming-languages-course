require_relative './graphics'

class Piece
  def initialize(point_array, board)
    @all_rotations = point_array
    @rotation_index = (0..(@all_rotations.size - 1)).to_a.sample
    @color = All_Colors.sample
    @base_position = [5, 0]
    @board = board
    @moved = true
  end

  def current_rotation
    @all_rotations[@rotation_index]
  end

  attr_reader :moved

  def position
    @base_position
  end

  attr_reader :color

  def drop_by_one
    @moved = move(0, 1, 0)
  end

  def move(delta_x, delta_y, delta_rotation)
    moved = true
    potential = @all_rotations[(@rotation_index + delta_rotation) % @all_rotations.size]
    potential.each do |posns|
      unless @board.empty_at([posns[0] + delta_x + @base_position[0],
                              posns[1] + delta_y + @base_position[1]])
        moved = false
      end
    end
    if moved
      @base_position[0] += delta_x
      @base_position[1] += delta_y
      @rotation_index = (@rotation_index + delta_rotation) % @all_rotations.size
    end
    moved
  end

  def self.rotations(point_array)
    rotate1 = point_array.map { |x, y| [-y, x] }
    rotate2 = point_array.map { |x, y| [-x, -y] }
    rotate3 = point_array.map { |x, y| [y, -x] }
    [point_array, rotate1, rotate2, rotate3]
  end

  def self.next_piece(board)
    Piece.new(All_Pieces.sample, board)
  end

  All_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]], # square (only needs one)
                rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2]]],
                rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])].freeze # Z

  All_Colors = ['DarkGreen', 'dark blue', 'dark red', 'gold2', 'Purple3',
                'OrangeRed2', 'LightSkyBlue'].freeze
end

class Board
  def initialize(game)
    @grid = Array.new(num_rows) { Array.new(num_columns) }
    @current_block = Piece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def block_size
    15
  end

  def num_columns
    10
  end

  def num_rows
    27
  end

  attr_reader :score, :delay

  def game_over?
    @grid[1].any?
  end

  def run
    ran = @current_block.drop_by_one
    unless ran
      store_current
      next_piece unless game_over?
    end
    @game.update_score
    draw
  end

  def move_left
    @current_block.move(-1, 0, 0) if !game_over? && @game.running?
    draw
  end

  def move_right
    @current_block.move(1, 0, 0) if !game_over? && @game.running?
    draw
  end

  def rotate_clockwise
    @current_block.move(0, 0, 1) if !game_over? && @game.running?
    draw
  end

  def rotate_counter_clockwise
    @current_block.move(0, 0, -1) if !game_over? && @game.running?
    draw
  end

  def drop_all_the_way
    return unless @game.running?

    ran = @current_block.drop_by_one
    @current_pos.each(&:remove)
    while ran
      @score += 1
      ran = @current_block.drop_by_one
    end
    draw
    store_current
    next_piece unless game_over?
    @game.update_score
    draw
  end

  def next_piece
    @current_block = Piece.next_piece(self)
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position

    (0..3).each  do |index|
      current = locations[index]
      x = current[1] + displacement[1]
      y = current[0] + displacement[0]
      @grid[x][y] = @current_pos[index]
    end

    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def empty_at(point)
    return false unless (point[0] >= 0) && (point[0] < num_columns)

    return true if point[1] < 1

    return false if point[1] >= num_rows

    @grid[point[1]][point[0]].nil?
  end

  def remove_filled
    (2..(@grid.size - 1)).each do |num|
      next unless @grid[num].all?

      (0..(num_columns - 1)).each do |index|
        @grid[num][index].remove
        @grid[num][index] = nil
      end

      ((@grid.size - num + 1)..(@grid.size)).each do |num2|
        @grid[@grid.size - num2].each { |rect| rect&.move(0, block_size) }
        @grid[@grid.size - num2 + 1] = Array.new(@grid[@grid.size - num2])
      end

      @grid[0] = Array.new(num_columns)
      @score += 10
    end
    self
  end

  def draw
    @current_pos = @game.draw_piece(@current_block, @current_pos)
  end
end

class Tetris
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = Board.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    @root.bind('n', proc { new_game })

    @root.bind('p', proc { pause })

    @root.bind('q', proc { exitProgram })

    @root.bind('a', proc { @board.move_left })
    @root.bind('Left', proc { @board.move_left })

    @root.bind('d', proc { @board.move_right })
    @root.bind('Right', proc { @board.move_right })

    @root.bind('s', proc { @board.rotate_clockwise })
    @root.bind('Down', proc { @board.rotate_clockwise })

    @root.bind('w', proc { @board.rotate_counter_clockwise })
    @root.bind('Up', proc { @board.rotate_counter_clockwise })

    @root.bind('space', proc { @board.drop_all_the_way })
  end

  def buttons
    pause = TetrisButton.new('pause', 'lightcoral') { self.pause }
    pause.place(35, 50, 90, 7)

    new_game = TetrisButton.new('new game', 'lightcoral') { self.new_game }
    new_game.place(35, 75, 15, 7)

    quit = TetrisButton.new('quit', 'lightcoral') { exitProgram }
    quit.place(35, 50, 140, 7)

    move_left = TetrisButton.new('left', 'lightgreen') { @board.move_left }
    move_left.place(35, 50, 27, 536)

    move_right = TetrisButton.new('right', 'lightgreen') { @board.move_right }
    move_right.place(35, 50, 127, 536)

    rotate_clock = TetrisButton.new('^_)', 'lightgreen') { @board.rotate_clockwise }
    rotate_clock.place(35, 50, 77, 501)

    rotate_counter = TetrisButton.new('(_^', 'lightgreen') do
      @board.rotate_counter_clockwise
    end
    rotate_counter.place(35, 50, 77, 571)

    drop = TetrisButton.new('drop', 'lightgreen') { @board.drop_all_the_way }
    drop.place(35, 50, 77, 536)

    label = TetrisLabel.new(@root) do
      text 'Current Score: '
      background 'lightblue'
    end
    label.place(35, 100, 26, 45)
    @score = TetrisLabel.new(@root) do
      background 'lightblue'
    end
    @score.text(@board.score)
    @score.place(35, 50, 126, 45)
  end

  def new_game
    @canvas.unplace
    @canvas.delete
    set_board
    @score.text(@board.score)
    @running = true
    run_game
  end

  def pause
    if @running
      @running = false
      @timer.stop
    else
      @running = true
      run_game
    end
  end

  def update_score
    @score.text(@board.score)
  end

  def run_game
    return unless !@board.game_over? && @running

    @timer.stop
    @timer.start(@board.delay, (proc { @board.run; run_game }))
  end

  def running?
    @running
  end

  def draw_piece(piece, old = nil)
    old.each(&:remove) if !old.nil? && piece.moved
    size = @board.block_size
    blocks = piece.current_rotation
    start = piece.position
    blocks.map do |block|
      TetrisRect.new(@canvas, start[0] * size + block[0] * size + 3,
                     start[1] * size + block[1] * size,
                     start[0] * size + size + block[0] * size + 3,
                     start[1] * size + size + block[1] * size,
                     piece.color)
    end
  end
end

srand
