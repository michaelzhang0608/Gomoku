open ANSITerminal

type board = string array array

type spot = Black of (int * int) | White of (int * int) | Empty of (int * int)

type player_id = int

type score = {
  player1: int;
  player2: int
}

type player = {
  id: string;
  games_won: int;
  is_turn: bool;
  color: string;
  mutable pieces: (int * int) list;
  mutable score: int;
}



type game = {
  id: int;
  game_board: board;
  player1: player;
  player2: player;
}

let create_board = 
  Array.make_matrix 19 19 " + "

let print_color (board: board)  = 
  let print_line (line: string array) = 
    let print_piece (piece: string)  = 
      if piece = " M " then print_string [magenta] " O "
      else if piece = " B " then print_string [blue] " O "
      else if piece = " G " then print_string [green] " O "
      else if piece = " R " then print_string [red] " O "
      else if piece = " Y " then print_string [yellow] " O "
      else if piece = " X " then print_string [black] " O "
      else print_string [white] " + " in
    for index = 0  to Array.length line - 1 do 
      print_piece (Array.get line index) 
    done in
  Array.iter (fun line -> print_line line; print_endline " ") board

let make_move board x y player = 
  let piece = player.color in
  let line = Array.get board (y - 1) in
  Array.set line (x - 1) piece;
  Array.set board (y - 1) line;
  {player with pieces = player.pieces @ [(x,y)]}

(* see what pieces are captured *)
let rec dfs board x y enemy= 
  if Array.get (Array.get board (x)) (y) = enemy.color then true
  else if x < 0 || x > 18 || y < 0 || y > 18 then false
  else dfs board (x + 1) y enemy && dfs board x (y+1) enemy && 
       dfs board (x-1) y enemy && dfs board x (y-1) enemy

(* score points *)
let rec dfs2  board x y player = 
  if Array.get (Array.get board (x)) (y) <> " + " then begin
    if Array.get (Array.get board (x)) (y) = player.color then true 
    else false end
  else if x < 0 || x > 18 || y < 0 || y > 18 then true
  else dfs2 board (x + 1) y player && dfs2 board x (y+1) player &&
       dfs2 board (x-1) y player && dfs2 board x (y-1) player



let rec remove_from_list element lst = 
  match lst with
  | hd :: tl -> if hd = element then tl else remove_from_list element tl
  | [] -> []

let capture_pieces x y board player1= 
  let iter_function element = 
    match element with
    | (x, y) -> 
      if dfs board x y player1 then 
        let line = Array.get board (y - 1) in
        Array.set line (x - 1) "XX";
        Array.set board (y - 1) line in
  List.iter iter_function player1.pieces 


let capture_pieces2 x y board player2= 
  let iter_function element = 
    match element with
    | (x, y) -> 
      if dfs board x y player2 then 
        let line = Array.get board (y - 1) in
        Array.set line (x - 1) " YY ";
        Array.set board (y - 1) line in
  List.iter iter_function player2.pieces 

let rec get_empty_spots board acc x y = 
  if x = Array.length board  then acc
  else if Array.get(Array.get board (x)) (y) = " + " then 
    if y = Array.length board - 1 then 
      get_empty_spots board (acc @ [(x,y)]) (x + 1) 0
    else get_empty_spots board (acc @ [(x,y)]) x (y + 1)
  else begin
    if y = Array.length board - 1 then get_empty_spots board acc (x + 1) 0
    else get_empty_spots board acc x (y + 1) end


let find_winner board player1 player2= 
  let empties = get_empty_spots board [] 0 0 in
  let map_function element = 
    match element with
    | (x, y) -> 
      if dfs2 board x y player1 then 1
      else 0 in
  let map_function2 element = 
    match element with
    | (x, y) -> 
      if dfs2 board x y player2 then 1
      else 0 in
  let p1 = List.filter (fun x -> x = 1) (List.map map_function empties) in
  let p2 = List.filter (fun x -> x = 1) (List.map map_function2 empties) in
  let map_function3 element = 
    match element with
    | (x, y) -> 
      if Array.get (Array.get board x) y = "XX" then 1
      else 0 in
  let map_function4 element = 
    match element with
    | (x, y) -> 
      if Array.get (Array.get board x) y = "YY" then 1
      else 0 in
  let captured_pieces = List.filter (fun x -> x = 1) 
      (List.map map_function3 empties) in
  let captured_pieces2 = List.filter (fun x -> x = 1) 
      (List.map map_function4 empties) in
  let score1 = List.length p1 - List.length captured_pieces in
  let score2 = List.length p2 - List.length captured_pieces2 in
  if score1 > score2 then player1
  else player2
