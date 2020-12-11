open Game

module Tuple = struct 
  type t = int list
  (* use Pervasives compare *)
  let compare = compare
end


module Set = Set.Make(Tuple)

let get_length board x y number = 
  let color = Array.get (Array.get board (x)) (y) in
  if ((Game.dfs board (x - 1) y 1 color "north") + 
      (Game.dfs board (x + 1) y 1 color "south")) >= number ||
     ((Game.dfs board x (y - 1) 1 color "west") + 
      (Game.dfs board x (y + 1) 1 color "east")) >= number ||
     ((Game.dfs board (x - 1) (y - 1) 1 color "northwest") + 
      (Game.dfs board (x + 1) (y + 1) 1 color "southeast")) 
     >=number || ((Game.dfs board (x - 1) (y + 1) 1 color "northeast") +
                  (Game.dfs board (x + 1) (y - 1) 1 color "southwest")) 
                 >= number then true
  else false



(* more positive if in favor of player A, more negative if in favor of player B *)

let in_a_row board player number= 
  let f2 y element2 = 
    let f1 x element1 = 
      if Array.get(Array.get board x) y = player.color then 
        get_length board x y number
      else false in
    Array.mapi f1 element2 in
  let array = Array.mapi f2 board in
  let list = Array.fold_left 
      (fun acc element -> (Array.to_list element)) [] array in
  List.mem true list

let evaluation board player1 player2 = 
  if in_a_row board player2 4 then begin
    if in_a_row board player1 5 then 50
    else if in_a_row board player1 4 then 12
    else if in_a_row board player1 3 then -12
    else if in_a_row board player1 2 then -26
    else if in_a_row board player1 1 then -35
    else -50 end
  else begin
    if in_a_row board player1 5 then 100
    else if in_a_row board player1 4 then 62
    else if in_a_row board player1 3 then 38
    else if in_a_row board player1 2 then 24
    else if in_a_row board player1 1 then 15
    else 1  end


let terminal_node board boardx boardy botx boty = 
  Game.check_victor board boardx boardy || Game.check_victor board botx boty
  || Game.check_tie board

let check_empty board x y acc = 
  if Array.get(Array.get board x) y = " + " then Set.add [x;y] acc
  else acc


let get_empty_spots board = 
  let f2 y element2 = 
    let f1 x element1 = 
      if Array.get(Array.get board x) y = " + " then (x,y) 
      else (-1, -1) in
    Array.mapi f1 element2 in
  let array = Array.mapi f2 board in
  let array = Array.fold_left (fun acc element -> (Array.to_list element)) [] array in
  let filter_function x = 
    match x with
    | (x,y) -> if x = -1 then false else true in
  List.filter filter_function array 

let rec get_empty_spots board acc x y = 
  if x  = Array.length board  then acc
  else if Array.get(Array.get board (x)) (y) = " - " then 
    if y = Array.length board - 1 then get_empty_spots board (acc @ [(x,y)]) (x + 1) 0
    else get_empty_spots board (acc @ [(x,y)]) x (y + 1)
  else begin
    if y = Array.length board - 1 then get_empty_spots board acc (x + 1) 0
    else get_empty_spots board acc x (y + 1) end




let make_move2 board x y (player: player)= 
  let piece = player.color in
  let line = Array.get board (x - 1) in
  Array.set line (y - 1) piece;
  Array.set board (x - 1) line;
  board;;



(** function minimax(node, depth, maximizingPlayer) is
    if depth = 0 or node is a terminal node then
        return the heuristic value of node
    if maximizingPlayer then
        value := −∞
        for each child of node do
            value := max(value, minimax(child, depth − 1, FALSE))
        return value
    else (* minimizing player *)
        value := +∞
        for each child of node do
            value := min(value, minimax(child, depth − 1, TRUE))
        return value **)
(** let rec minimax depth board player1 player2 max_or_min = 
    match depth with
    | 0 -> (None, evaluation board player1 player2)
    | _ -> 
    let empty_spots = get_empty_spots board in
    match empty_spots with
    | [] -> (None, evaluation board player1 player2)
    | _ -> 
      if max_or_min then begin
        let rec get_max lst acc board player1 player2= 
          match lst with
          | [] -> acc
          | hd :: tl -> 
            match hd with
            | (-1, -1) -> get_max tl acc board player1 player2
            | (x, y) -> 
              let board2 = board in
              let board2 = make_move2 board2 x y player1 in
              match minimax (depth - 1) board2 player1 player2 false with
              | (_, num)->  
                match acc with
                | (_, acc_val) -> 
                if num > acc_val then 
                  get_max tl (minimax (depth - 1) board2 player1 player2 false) board player1 player2
                else get_max tl acc board player1 player2 in
        get_max (get_empty_spots board) (None, Int.of_float(Float.neg_infinity)) board player1 player2 end
      else **)

let rec get_max lst acc board player1 player2 = 
  match lst with
  | [] -> acc
  | hd :: tl -> 
    match hd with
    | (x, y) -> 
      let board2 = board in
      let board2 = make_move2 board2 x y player1 in
      match acc with
      | (_, num) -> 
        if evaluation board2 player1 player2 > num then 
          get_max tl ((x,y),evaluation board player1 player2) board player1 player2
        else get_max tl acc board player1 player2


let get_optimal_move board player bot= 
  let empties = get_empty_spots board [] 0 0 in
  match get_max empties (List.hd empties,-100) board bot player with
  | ((x,y),a) -> 
    (x,y)




















