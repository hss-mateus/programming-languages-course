require_relative './provided'
require_relative '../homework'

def run_tetris
  Tetris.new
  main_loop
end

def run_my_tetris
  MyTetris.new
  main_loop
end

if ARGV.count.zero?
  run_my_tetris
elsif ARGV.count != 1
  puts 'usage: hw6runner.rb [enhanced | original]'
elsif ARGV[0] == 'enhanced'
  run_my_tetris
elsif ARGV[0] == 'original'
  run_tetris
else
  puts 'usage: hw6runner.rb [enhanced | original]'
end
