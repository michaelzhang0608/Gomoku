type board = string array array

type score

type player

type player_id

type game

(** [print_board board] is the visual representation of the Gomoku board, using
    the ‘+’ character for empty spots. Player 1’s stones are represented by the
    char '○' and Player 2’s stones are represented by the char '•'.
    The board [board] is the current board with all the players' moves. *)
val print_board : board -> unit

(** [make_move board x y] updates the game board with the player’s move. The
    board [board] represents the game board the players are adding a move to.
    The int [x] is the x coordinate of the board and the int [y] is the y
    coordinate of the board. The players must take turns to make moves to
    maintain the order between Player 1 and Player 2’s stones. *)
val make_move :  board -> bool -> board

(** [get_victor board] is the player_id of the winner of game has a winner or a
    tie. The board [board] is the current board with all the players' moves. *)
val  get_victor: board -> player_id

(** [print_winner if_win] is a string, indicating which player won or if there
    was a tie. When the bool [if_win] equal true, the victory is printed. 
    Otherwise, the game continues. *)
val  print_winner : bool -> string

(** [update_score board] is of type score and keeps track of how many games
    each player has won in a session. The score [score] represents the current
     points earned by each player.  *)
val update_score : score -> score
