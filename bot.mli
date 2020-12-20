(** 
   Representation of the bot player for Gomoku.

   This module represents the bot and functions that result in bot moves of
   varying difficulty.
*)

open Game

(** [check_length] uses tail-recursion to check the length of a 
    group of stones in the direction of [direction] *)
val check_length: board -> int -> int -> string -> string -> int -> int


(** [make_temporary_move] makes a potential move for the bot *)
val make_temporary_move: board -> int -> int -> Game.player -> unit

(** [clear_piece] undos the temporary move made in [make_temporary_move] *)
val clear_piece: board -> int -> int -> unit

(** [find_heuristic board x y] returns a list of two elements [el1, el2] such 
    that el1 is the maximum number of pieces in a row that the piece at 
    coordinates x and y belongs to, and el2 is the total number of pieces that 
    are either next to the piece at coordinates x y or are connected to it *)
val find_heuristic: board -> int -> int -> Game.player -> int list

(** [check_edge_cases board x y player] returns the number of pieces in a row 
    that would be the player's color if a piece of the player's color 
    was placed at coordinates x and y. *)
val check_edge_cases: board -> int -> int -> Game.player -> int


(** [get_optimal_move board player bot difficulty] is the tuple int * int that
    represents the x coordinate and y coordinate of the optimal move to play
    given the current game. The board [board] is the current game board with
    all the previous moves. The player [player] represents the human player.
    The player [bot] represents the bot player. The string [difficulty] 
    represents how smart the bot will play (easy, medium, hard), which
    influences the optimal move.*)
val get_optimal_move: string array array -> player -> player -> string ->
  (int * int)

(** [save_bot_players player name bot bool difficulty] updates or creates a 
    CSV file with the specified player and bot. The player [player] 
    represents the human player. The bot [bot] represents the bot player
    The bool [bool] represents which players goes first. True means the human
    player goes first and false means the bot goes first. 
    The string [difficulty] represents the level of difficulty for the bot. 
    The string [name] represents the name of the CSV file to save the 
    bot player information to. If the file name exists, the file is update, 
    otherwise a new file of that name is created.

    The string [name] must end with ".csv" in order to create a valid file.*)
val save_bot_players: Game.player -> Game.player -> string -> bool -> 
  string -> unit


(* 

let find_max = function
  | x::xs -> List.fold_left max x xs

let bot_4 board x y bot human = 
  1000 + List.nth (find_heuristic board x y bot) 1 + 
  List.nth (find_heuristic board x y human) 1

let human_4 board x y bot human = 
  1000 + List.nth (find_heuristic board x y human) 1 + 
  List.nth (find_heuristic board x y bot) 1

let bot_3 board x y bot human = 
  800 + List.nth (find_heuristic board x y bot) 1 + 
  List.nth (find_heuristic board x y human) 1

let human_3 board x y bot human = 
  600 + List.nth (find_heuristic board x y human) 1 +
  List.nth (find_heuristic board x y bot) 1

let bot_2 board x y bot human = 
  400 + List.nth (find_heuristic board x y bot) 1 + 
  List.nth (find_heuristic board x y human) 1

let human_2 board x y bot human = 
  200 + List.nth (find_heuristic board x y human) 1 + 
  List.nth (find_heuristic board x y bot) 1

let bot_1 board x y bot human = 
  100 + List.nth (find_heuristic board x y bot) 1 + 
  List.nth (find_heuristic board x y human) 1

let human_1 board x y bot human = 
  50 + List.nth (find_heuristic board x y human) 1 + 
  List.nth (find_heuristic board x y bot) 1




let evaluation board (bot:Game.player) human = 
  let x = List.nth bot.last_move 0 in
  let y = List.nth bot.last_move 1 in
  if List.hd (find_heuristic board x y bot) >= 6 || check_edge_cases board 
       x y bot >= 6 then bot_4 board x y bot human
  else if List.hd (find_heuristic board x y human ) >= 6 ||check_edge_cases 
            board x y human  >= 6 then human_4 board x y bot human
  else if List.hd (find_heuristic board x y bot) = 5 || check_edge_cases board 
            x y bot = 5 then bot_3 board x y bot human
  else if List.hd (find_heuristic board x y human) >= 5 || check_edge_cases 
            board x y human >= 5 then human_3 board x y bot human
  else if List.hd (find_heuristic board x y bot) = 4 || check_edge_cases board 
            x y bot  = 4 then bot_2 board x y bot human
  else if List.hd (find_heuristic board x y human) = 4 || check_edge_cases board
            x y human = 4 then human_2 board x y bot human
  else if List.hd (find_heuristic board x y bot) = 3 || check_edge_cases board 
            x y bot = 3 then bot_1 board x y bot human
  else if List.hd (find_heuristic board x y human) = 3 || check_edge_cases board
            x y human = 3 then human_1 board x y bot human
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
      (x,y) *)