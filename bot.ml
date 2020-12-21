open Game

(** The helper function [check_length] uses tail-recursion to check the 
    length of a group of stones in the direction of [direction] *)
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

(** The helper function [make_temporary_move] makes a potential move for 
    the bot *)
let make_temporary_move board x y (bot: Game.player)= 
  let piece = bot.color in
  let line = Array.get board x in
  Array.set line y piece;
  Array.set board x line;;


(** The helper function [clear_piece] undos the temporary move made in 
    [make_temporary_move] *)
let clear_piece board x y = 
  let piece = " - " in
  let line = Array.get board x in
  Array.set line y piece;
  Array.set board x line;;


let find_max = function
  | x::xs -> List.fold_left max x xs
  | _ -> failwith "Not valid"


(** The helper function [find_heuristic board x y] returns a list of two 
    elements [el1, el2] such that el1 is the maximum number of pieces in a row 
    that the piece at coordinates x and y belongs to, and el2 is the total 
    number of pieces that affected by the new piece (are adjacent to it) *)
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


(** The helper function [check_edge_cases board x y player] checks an 
    edge case where there is a spot purposely left empty in the 
    middle of a string of pieces that, 
    once filled, would give the player a certain number of pieces in a row. This
    is a common strategy for experienced Gomoku players . *)
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

(* [bot_4] returns the heuristic for the case in which the bot has 4 pieces in a 
   row already and could win on this turn *)
let bot_4 board x y bot human difficulty= 
  if difficulty = "hard" then 1000 + List.nth (find_heuristic board x y bot) 1 
                              + List.nth (find_heuristic board x y human) 1
  else 1000 + List.nth (find_heuristic board x y bot) 1

(* [human_4] returns the heuristic for a case in which the human has 4 pieces
   in a row already and is about to win on their turn and needs to be blocked *)
let human_4 board x y bot human difficulty= 
  if difficulty = "hard" then 1000 + List.nth (find_heuristic board x y human)
                                1 + List.nth (find_heuristic board x y bot) 1
  else 1000 + List.nth (find_heuristic board x y human) 1

(* [bot_3] returns the heuristic for the case where the bot has 3 pieces in
   a row already and could get four in a row *)
let bot_3 board x y bot human difficulty= 
  if difficulty = "hard" then 800 + List.nth (find_heuristic board x y bot) 1 + 
                              List.nth (find_heuristic board x y human) 1
  else 800 + List.nth (find_heuristic board x y bot) 1

(* [human_3] returns the heuristic for the case where the human has 3 pieces
   in a row and could get four in a row *)
let human_3 board x y bot human difficulty= 
  if difficulty = "hard" then 600 + List.nth (find_heuristic board x y human) 1 
                              +List.nth (find_heuristic board x y bot) 1
  else 600 + List.nth (find_heuristic board x y human) 1

(* [bot_2] returns the heuristic for the case where the bot has 2 pieces
   in a row already and could get 3 in a row *)
let bot_2 board x y bot human difficulty= 
  if difficulty = "hard" then 400 + List.nth (find_heuristic board x y bot) 1 + 
                              List.nth (find_heuristic board x y human) 1
  else 400 + List.nth (find_heuristic board x y bot) 1

(* [human_2] returns the heuristic for the case where the human has a piece
   next to the possible move and could get 2 in a row *)
let human_2 board x y bot human difficulty= 
  if difficulty = "hard" then 
    200 + List.nth (find_heuristic board x y human) 1 + 
    List.nth (find_heuristic board x y bot) 1
  else 200 + List.nth (find_heuristic board x y human) 1 

(* [bot_1] returns the heuristic for the case where the max amount of pieces
   in a row the bot could get with the current possible move is one *)
let bot_1 board x y bot human difficulty= 
  if difficulty = "hard" then 100 + List.nth (find_heuristic board x y bot) 1 + 
                              List.nth (find_heuristic board x y human) 1
  else 100 + List.nth (find_heuristic board x y bot) 1

(* [human_1] returns the heuristic for the case where the max amount of pieces
   in a row the human could get with the current possible move is one *)
let human_1 board x y bot human difficulty= 
  if difficulty = "hard" then 50 + List.nth (find_heuristic board x y human) 1 + 
                              List.nth (find_heuristic board x y bot) 1
  else 50 + List.nth (find_heuristic board x y human) 1


let hard_evaluation board (bot:Game.player) human difficulty = 
  let x = List.nth bot.last_move 0 in
  let y = List.nth bot.last_move 1 in
  if List.hd (find_heuristic board x y bot) >= 6 || check_edge_cases board 
       x y bot >= 6 then bot_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human ) >= 6 ||check_edge_cases 
            board x y human  >= 6 then human_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 5 || check_edge_cases board 
            x y bot = 5 then bot_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) >= 5 || check_edge_cases 
            board x y human >= 5 then human_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 4 || check_edge_cases board 
            x y bot  = 4 then bot_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 4 || check_edge_cases board
            x y human = 4 then human_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 3 || check_edge_cases board 
            x y bot = 3 then bot_1 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 3 || check_edge_cases board
            x y human = 3 then human_1 board x y bot human difficulty
  else 5 


let medium_evaluation board (bot:Game.player) human difficulty= 
  let x = List.nth bot.last_move 0 in
  let y = List.nth bot.last_move 1 in
  if List.hd (find_heuristic board x y bot) >= 6 || check_edge_cases board 
       x y bot >= 6 then bot_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human ) >= 6 ||check_edge_cases 
            board x y human  >= 6 then human_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 5 || check_edge_cases board 
            x y bot = 5 then bot_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) >= 5 || check_edge_cases 
            board x y human >= 5 then human_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 4 || check_edge_cases board 
            x y bot  = 4 then bot_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 4 || check_edge_cases board
            x y human = 4 then human_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 3 || check_edge_cases board 
            x y bot = 3 then bot_1 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 3 || check_edge_cases board
            x y human = 3 then human_1 board x y bot human difficulty
  else 5 


let easy_evaluation board (bot:Game.player) human difficulty = 
  let x = List.nth bot.last_move 0 in
  let y = List.nth bot.last_move 1 in
  if List.hd (find_heuristic board x y bot) >= 6 then 
    bot_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human ) >= 6 
  then human_4 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 5 
  then bot_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) >= 5 
  then human_3 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 4 
  then bot_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 4 
  then human_2 board x y bot human difficulty
  else if List.hd (find_heuristic board x y bot) = 3 
  then bot_1 board x y bot human difficulty
  else if List.hd (find_heuristic board x y human) = 3 
  then human_1 board x y bot human difficulty
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


let rec get_move lst max board (player: Game.player) bot difficulty= 
  match lst with 
  | [] -> max
  | hd :: tl -> 
    match hd with 
    | (x, y) -> begin 
        make_temporary_move board x y bot;
        let potential_bot : Game.player = {bot with last_move = [x;y]} in 
        evaluate_move max board player bot potential_bot difficulty x y tl end


and evaluate_move max board player bot potential_bot difficulty x y tl = 
  match max with 
  | (_, num) -> 
    let new_max = 
      if difficulty = "hard" then 
        hard_evaluation board potential_bot player difficulty
      else if difficulty = "medium" then 
        medium_evaluation board potential_bot player  difficulty
      else easy_evaluation board potential_bot player difficulty in
    clear_piece board x y;
    if new_max > num then 
      get_move tl ((x,y), new_max) board player
        potential_bot difficulty
    else 
      get_move tl max board player bot difficulty

let get_optimal_move board player bot difficulty= 
  if bot.last_move = [-1;-1] then 
    if Array.length board = 13 && Array.get(Array.get board 6) 6 = " - "
    then (6,6) 
    else if Array.length board = 15 && Array.get(Array.get board 7) 7 = " - "
    then (7,7)
    else 
      let empties = get_empty_spots board [] 0 0 in
      match get_move empties (List.hd empties,-100) 
              board player bot difficulty with
      | ((x,y),a) -> 
        (x,y)
  else 
    let empties = get_empty_spots board [] 0 0 in
    match get_move empties (List.hd empties,-100) 
            board player bot difficulty with
    | ((x,y),a) -> 
      (x,y)







