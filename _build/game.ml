open ANSITerminal

type board = string array array

type player_id = int

type score = {
  player1: int;
  player2: int
}

type player = {
  id: int;
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

let make_move board x y (player: player)= 
  let piece = player.color in
  let line = Array.get board (y - 1) in
  Array.set line (x - 1) piece;
  Array.set board (y - 1) line;;


let rec dfs board x y acc color dir= 
  if y < 0 || y = Array.length board || x < 0 || x = Array.length board 
     || Array.get(Array.get board (x)) (y) <> color then acc
  else if dir = "north" then dfs board (x - 1) y (acc + 1) color dir
  else if dir = "east" then dfs board x (y + 1) (acc + 1) color dir 
  else if dir = "west" then dfs board x (y - 1) (acc + 1) color dir
  else if dir = "south" then dfs board (x + 1) y (acc + 1) color dir
  else if dir = "northwest" then dfs board (x - 1) (y - 1) (acc + 1) color dir
  else if dir = "northeast" then dfs board (x - 1) (y + 1) (acc + 1) color dir
  else if dir = "southeast" then dfs board (x + 1) (y + 1) (acc + 1) color dir
  else dfs board (x + 1) (y - 1) (acc + 1) color dir


let check_victor board x y = 
  let color = Array.get (Array.get board (x)) (y) in
  if ((dfs board (x - 1) y 1 color "north") + 
      (dfs board (x + 1) y 1 color "south")) >= 6 ||
     ((dfs board x (y - 1) 1 color "west") + 
      (dfs board x (y + 1) 1 color "east")) >= 6 ||
     ((dfs board (x - 1) (y - 1) 1 color "northwest") + 
      (dfs board (x + 1) (y + 1) 1 color "southeast")) 
     >=6 || ((dfs board (x - 1) (y + 1) 1 color "northeast") +
             (dfs board (x + 1) (y - 1) 1 color "southwest")) >= 6 then true
  else false

let check_tie board = 
  (Array.for_all (fun line ->
       Array.for_all ((<>) " + ") line) board)

let print_winner winner = 
  failwith "unimplemented"

let update_score score = 
  failwith "unimplemented"

let get_turn player = 
  player.is_turn

let get_id (player :player) = 
  player.id

let get_games_won player = 
  player.games_won

let update_games_won player = 
  {player with games_won = player.games_won + 1}

let change_turn player = 
  if get_turn player = true then 
    {player with is_turn = false}
  else {player with is_turn = true}