(** 
   Representation of the bot player for Gomoku.

   This module represents the bot and functions that result in bot moves of
   varying difficulty.
*)

open Game

(** [get_optimal_move board player bot difficulty] is the tuple int * int that
    represents the x coordinate and y coordinate of the optimal move to play
    given the current game. The board [board] is the current game board with
    all the previous moves. The player [player] represents the human player.
    The player [bot] represents the bot player. The string [difficulty] 
    represents how smart the bot will play (easy, medium, hard), which
    influences the optimal move.*)
val get_optimal_move: string array array -> player -> player -> string ->
  (int * int)
