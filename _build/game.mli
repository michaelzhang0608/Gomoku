(** 
   Representation of the Gomoku game.

   This module represents the board, players, moves that occur during the
   game, winners, and functions that result in player moves.
*)

type board = string array array

type score

type player = {
  id: string;
  games_won: int;
  is_turn: bool;
  color: string;
  last_move: int list;
}

type player_id

type game

(** [print_board board] is the visual representation of the Gomoku board, using
    the ‘+’ character for empty spots. Player 1’s stones are represented by the
     char 'O', colored according to Player 1's color, and Player 2’s stones 
     are represented by the char 'O', colored according to Player 2's color.
    The board [board] is the current board with all the players' moves. *)
val print_color : board -> unit

(** [make_move board x y] updates the game board with the player’s move. The
    board [board] represents the game board the players are adding a move to.
    The int [x] is the x coordinate of the board and the int [y] is the y
    coordinate of the board. The players must take turns to make moves to
    maintain the order between Player 1 and Player 2’s stones. *)
val make_move : string array array -> int -> int -> player -> unit

(** [check_victor board] is the bool if the game has a winner. The board [board]
    is the current board with all the players' moves. *)
val  check_victor: 'a array array -> int -> int -> bool

(** [check_tie board] is the bool if the game has resulted in a tie. The board 
    [board] is the current board with all the players' moves. A tie only occurs 
    if the board is filled, meaning there are no more moves to be made and there
    is no winner.*)
val  check_tie: string array array -> bool

(** [get_turn player] is the bool representing if the turn belongs to the 
    player. The player [player] represents a player in the game.  *)
val get_turn: player -> bool

(** [get_turn player] is the int representing player's id number. The player 
    [player] represents a player in the game.  *)
val get_id: player -> string

(** [get_games_won player] is the int representing the number of games
    this player has won. The player [player] represents a player in the game. *)
val get_games_won: player -> int

(** [update_score player] is the updated player [player] that shows how many 
    games the player has won thus far. The player [player] represents the 
    player who won the previous game that gets a point added to their score.  *)
val update_games_won: player -> player

(** [update_score board] is of type score and keeps track of how many games
    each player has won in a session. The score [score] represents the current
     points earned by each player.  *)
val update_score: player -> player

(** [change_turn player] is the updated player [player] that shows how many 
    games the player has won thus far. The player [player] represents the 
    player who won the previous game that gets a point added to their score. *)
val change_turn: player -> player

(** [reset_board board] updates the board [board] to an empty board of the
    previous board's length. Empty spots are represented by the string " + ".
    The board [board] is the board with the players' moves. *)
val reset_board: board -> unit

(** [find_color color] is the string presentation of the color of a
    player's stones to be printed on the board. The string [color] is the 
    string representation for the color of a player's stones. *)
val find_color: string -> string

(** [create_board dimensions] create the string array array representing
    the game board. The int [dimensions] is the length of the game board. *)
val create_board: int -> string array array

(** [clear_board board] returns the string array array representing
    an empty game board. The board [board] represents the current game board
    with the players' moves. *)
val clear_board: 'a array array -> string array array

(** [available_colors color1] returns the list of string representations of 
    colors and each color's respective ANSITerminal style, except for the color 
    represented by [color1]. The string [color1] represents the color that 
    player 1 chose; therefore, player 2 cannot be allowed to choose the 
    same color.*)
val available_colors: 'a -> ('a * 'b) list -> 'c -> ('a * 'b) list

(** [load_game name] is the board saved on the CSV file. The string [name] 
    represents the CSV file name. *)
val load_game: string -> string array array

(** [save_name board player1 player2 first] updates the CSV file "board.csv" 
    with the specified game board. The string array array [board] represents
     the board that is to be saved ont the CSV file. The player [player1] and
     player [player2] represent the first and second players in the game,
     respectively. The player [first] represents the player whose turn is first.
     The dimensions of [board] must be 13 or 15, comprised of empty spots or 
     player moves. *)

(** [load_players name] are the players saved on the CSV file. The string [name] 
    represents the CSV file name. *)
val load_players: string -> player * bool * player * bool * bool * string

(** [save_human_players player1 player2] updates the CSV file "players.csv" with 
    the two specified players. The player [player1] represents first player and
    the player [player2] represents the second player in the game. *)
val save_human_players: player -> player -> string -> player -> unit


val save_board: string array array -> string -> unit

val load_bot_players: string list list -> player * bool * 
                                          player * bool * bool * string
