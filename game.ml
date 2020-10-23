open ANSITerminal

type board = string array array

type player_id = int

type player = {
  id: player_id;
  games_won: int;
  is_turn: bool;
  color: string
}

type game = {
  id: int;
  game_board: board;
  player1: player;
  player2: player;
}

(** let print_board (board: board) = 
    let print_line (line: string array) =  
    Array.fold_left (fun acc x -> acc ^ x) "" line in
    Array.iter(fun x -> print_endline (print_line x)) board **)

let print_color (board: board)  = 
  let print_line (line: string array) = 
    let print_piece (piece: string)  = 
      if piece = " B " then print_string [magenta] " O "
      else if piece = " W " then print_string [yellow] " O "
      else print_string [white] " + " in
    for index = 0  to Array.length line - 1 do 
      print_piece (Array.get line index) 
    done in
  Array.iter (fun line -> print_line line; print_endline " ") board
