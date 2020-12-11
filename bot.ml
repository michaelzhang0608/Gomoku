open Game




let rec check_length (board: Game.board) x y dir color acc = 
  if x < 0 || x = Array.length board || y < 0 || y = Array.length board || 
     Array.get (Array.get board (x)) (y) <> color then acc
  else if dir = "north" then 
    check_length board (x - 1) y "north" color (acc + 1)
  else if dir = "south" then 
    check_length board (x + 1) y "south" color (acc + 1)
  else if dir = "west" then check_length board x (y - 1) "west" color (acc + 1)
  else if dir = "east" then check_length board x (y + 1) "east" color (acc + 1)
  else if dir = "northwest" then check_length board (x - 1) (y - 1) "northwest"
      color (acc + 1)
  else if dir = "southeast" then check_length board (x + 1) (y + 1) "southeast"
      color (acc + 1)
  else if dir = "northeast" then check_length board (x - 1) (y + 1) "northeast"
      color (acc + 1)
  else check_length board (x + 1) (y - 1) "southwest"
      color (acc + 1)





let make_temporary_move board x y (bot: Game.player)= 
  let piece = bot.color in
  let line = Array.get board x in
  Array.set line y piece;
  Array.set board x line;;

let make_move_color board x y color = 
  let line = Array.get board x in
  Array.set line y color;
  Array.set board x line;;

let clear_piece board x y = 
  let piece = " - " in
  let line = Array.get board x in
  Array.set line y piece;
  Array.set board x line;;


let find_max = function
  | x::xs -> List.fold_left max x xs

(* returns number in a a row if player's piece was placed at x y *)
let find_heuristic board x y (player: Game.player) = 
  let color = player.color in
  let lst = [check_length board (x - 1) y "north" color 1;
             check_length board (x + 1) y "south" color 1;
             check_length board x (y + 1) "east" color 1;
             check_length board x (y - 1) "west" color 1;
             check_length board (x - 1) (y - 1) "northwest" color 1;
             check_length board (x - 1) (y + 1) "northeast" color 1;
             check_length board (x + 1) (y + 1) "southeast" color 1;
             check_length board (x + 1) (y - 1) "southwest" color 1] in
  let rec sum acc= function
    | [] -> acc
    | x :: xs -> sum (acc + x) xs in
  [find_max lst; sum 0 lst]



let check_edge_cases board x y player = 
  let color = player.color in
  let lst = [check_length board (x - 1) y "north" color 1 +
             check_length board (x + 1) y "south" color 1;
             check_length board x (y + 1) "east" color 1 +
             check_length board x (y - 1) "west" color 1;
             check_length board (x - 1) (y - 1) "northwest" color 1 +
             check_length board (x + 1) (y + 1) "southeast" color 1;
             check_length board (x - 1) (y + 1) "northeast" color 1 +
             check_length board (x + 1) (y - 1) "southwest" color 1] in
  find_max lst




let evaluation board (bot:Game.player) player = 
  let x = List.nth bot.last_move 0 in
  let y = List.nth bot.last_move 1 in
  (** bot has 4 in a row already or will get 5 in a row **)
  if List.hd (find_heuristic board x y bot) >= 6 || 
     check_edge_cases board x y bot >= 6 then 
    10000 + List.nth (find_heuristic board x y bot) 1 + 
    List.nth (find_heuristic board x y player) 1
    (** human has 4 in a row already or human will get 5 in a row **)
  else if List.hd (find_heuristic board x y player ) >= 6 ||
          check_edge_cases board x y player  >= 6 then
    1000 + List.nth (find_heuristic board x y player ) 1
    (* bot has 3 in a row already or bot will get 4 in a row *)
  else if List.hd (find_heuristic board x y bot) = 5 || 
          check_edge_cases board x y bot = 5 then 
    800 + List.nth (find_heuristic board x y bot) 1 + 
    List.nth (find_heuristic board x y player) 1
    (* human has 3 in a row already or human will get 4 in a row *)
  else if List.hd (find_heuristic board x y player ) >= 5 ||
          check_edge_cases board x y player >= 5 then 
    600 + List.nth (find_heuristic board x y player ) 1
    (* bot has 2 in a row already or bot will get 3 in a row *)
  else if List.hd (find_heuristic board x y bot) = 4 ||
          check_edge_cases board x y bot  = 4 then 
    400 + List.nth (find_heuristic board x y bot) 1 + 
    List.nth (find_heuristic board x y player) 1
    (* human has 2 in a row already or human will get 3 in a row *)
  else if List.hd (find_heuristic board x y player) = 4 ||
          check_edge_cases board x y player = 4 then 
    200 + List.nth (find_heuristic board x y player) 1
    (* bot will get 2 in a row *)
  else if List.hd (find_heuristic board x y bot) = 3 ||
          check_edge_cases board x y bot = 3 then 
    100 + List.nth (find_heuristic board x y bot) 1
    (* human will get 2 in a row *)
  else if List.hd (find_heuristic board x y player) = 3 || 
          check_edge_cases board x y player = 3 then
    50 + List.nth (find_heuristic board x y player )1 
  else 5 






let rec get_empty_spots board acc x y = 
  if x  = Array.length board  then acc
  else if Array.get(Array.get board (x)) (y) = " - " then 
    if y = Array.length board - 1 then 
      get_empty_spots board (acc @ [(x,y)]) (x + 1) 0
    else get_empty_spots board (acc @ [(x,y)]) x (y + 1)
  else begin
    if y = Array.length board - 1 then get_empty_spots board acc (x + 1) 0
    else get_empty_spots board acc x (y + 1) end



let rec get_move lst max board (player: Game.player) bot = 
  match lst with
  | [] -> max
  | hd :: tl -> 
    match hd with
    | (x, y) -> 
      make_temporary_move board x y bot;
      let potential_bot : Game.player = {bot with last_move = [x;y]} in
      match max with
      | (_, num) -> 
        let new_max = evaluation board potential_bot player in
        clear_piece board x y;
        if new_max > num then 
          get_move tl ((x,y), new_max) board player potential_bot
        else 
          get_move tl max board player bot


let get_optimal_move board player bot = 
  if bot.last_move = [-1;-1] then 
    if Array.length board = 13 && Array.get(Array.get board 6) 6 = " - "
    then (6,6) 
    else if Array.length board = 15 && Array.get(Array.get board 7) 7 = " - "
    then (7,7)
    else 
      let empties = get_empty_spots board [] 0 0 in
      match get_move empties (List.hd empties,-100) board player bot with
      | ((x,y),a) -> 
        (x,y)
  else 
    let empties = get_empty_spots board [] 0 0 in
    match get_move empties (List.hd empties,-100) board player bot with
    | ((x,y),a) -> 
      (x,y)




















