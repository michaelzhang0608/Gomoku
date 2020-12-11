

type board 

type spot

type player_id 

type score

type player = {
  id: string;
  games_won: int;
  is_turn: bool;
  color: string;
  mutable pieces: (int * int) list;
  mutable score: int;
}



type game

val create_board : string array array

val print_color: board -> unit

val make_move: board -> int -> int -> player -> player

val dfs: board -> int -> int -> player -> bool

val dfs2: board -> int -> int -> player -> bool

val capture_pieces: int -> int -> board -> player -> unit

val capture_pieces2: int -> int -> board -> player -> unit

val get_empty_spots: board -> (int * int) list -> int -> int -> (int * int) list

val find_winner: board -> player -> player -> player

