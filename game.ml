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

let place_stone board x y piece = 
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


let check_winner board x y = 
  let color = Array.get (Array.get board (x)) (y) in
  if ((dfs board (x - 1) y 1 color "north") + 
      (dfs board (x + 1) y 1 color "south")) >= 5 ||
     ((dfs board x (y - 1) 1 color "west") + 
      (dfs board x (y + 1) 1 color "east")) >= 5 ||
     ((dfs board (x - 1) (y - 1) 1 color "northwest") + 
      (dfs board (x + 1) (y + 1) 1 color "southeast")) 
     >=5 || ((dfs board (x - 1) (y + 1) 1 color "northeast") +
             (dfs board (x + 1) (y - 1) 1 color "southwest")) >= 5 then "true"
  else "false"




let print_ouput = 
  let board = [|[|" W "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " W "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " W "; " W "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " W "; " + "|];
                [|" B "; " B "; " B "; " B "; " B "; " W "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " W "; " W "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " W "; " W "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " B "; " + "; " + "|];
                [|" + "; " + "; " W "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " W "; " + "; " + "|];
                [|" + "; " + "; " W "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " W "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|];
                [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                  " + "; " + "; " + "; " + "; " + "|]|] in
  let print = print_color board in
  let pr  = print_int (dfs board 3 3 1 " B " "east") in
  check_winner board 3 4







