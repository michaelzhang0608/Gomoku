open Sys
open Game

let create_board dimension = 
  Array.make_matrix dimension dimension " - "

let clear_board board = 
  create_board (Array.length board) 

let rec play_again board p1 p2 =  
  print_endline "Would you like to play again? (Y/N)";
  print_string  "> ";
  match read_line () with
  | command -> 
    if command <> "Y" && command <> "N" && command <> "quit" then 
      play_again board p1 p2
    else if command = "Y" then true
    else false


let victory board p1 p2 winner= 
  Game.print_color board;
  print_endline ("Congratulations! Player " ^ 
                 string_of_int (Game.get_id winner) ^ " has won!");
  if p1 = winner then 
    play_again (clear_board board) p1 p2
  else 
    play_again (clear_board board) p1 p2

let print_coordinates length = 
  for index = 0 to (length - 1) do 
    print_string (" " ^ string_of_int (index) ^ " ");
  done;
  print_endline "";
  for index = 1 to (length - 1) do
    print_endline (string_of_int index);
    print_endline "";
  done


let rec get_x_coordinate length = 
  print_endline "Please enter the horizontal coordinate you want
  to place your piece in";
  print_string  "> ";
  try 
    let x = read_line() in
    if x = "quit" then -1
    else 
      let x = Stdlib.int_of_string (x) in
      if x < 0 || x > (length - 1) then 
        get_x_coordinate length
      else x
  with Failure _ -> 
    print_string "Invalid input, try again.";
    print_string  "> ";
    get_x_coordinate length

let rec get_y_coordinate length = 
  print_endline "Please enter the vertical coordinate you want
  to place your piece in";
  print_string  "> ";
  try 
    let y = read_line () in
    if y = "quit" then -1
    else 
      let y = Stdlib.int_of_string (read_line ()) in
      if y < 0 || y > (length - 1) then 
        get_x_coordinate length
      else y
  with Failure _ -> 
    print_string "Invalid input, try again.";
    print_string  "> ";
    get_y_coordinate length



let rec move (board : string array array) (p1: player) (p2:player) = 
  Game.print_color board;
  let x = get_x_coordinate (Array.length board) in
  if x = -1 then 
    print_endline "Bye have a beautiful time"; exit 0;
  let y = get_y_coordinate (Array.length board) in
  if y = -1 then 
    print_endline "Bye have a beautiful time"; exit 0;
  if Game.get_turn p1 then 
    (Game.make_move board x y p1; 
     if Game.check_victor board (y - 1) (x - 1)= true then 
       (if victory board p1 p2 p1 then 
          let new_p1 = Game.update_games_won p1 in 
          move (clear_board board) new_p1 p2;
        else print_endline "Bye have a beautiful time"; exit 0; )
     else 
       let new_p1 = Game.change_turn p1 in
       let new_p2 = Game.change_turn p2 in
       move board new_p1 new_p2;)
  else 
    (Game.make_move board x y p2;
     if Game.check_victor board (y - 1) (x - 1)= true then
       (if victory board p1 p2 p2 then 
          let new_p2 = Game.update_games_won p2 in
          move (clear_board board) p1 new_p2;
        else print_endline "Bye have a beautiful time"; exit 0;)
     else 
       let new_p1 = Game.change_turn p1 in
       let new_p2 = Game.change_turn p2 in
       move board new_p1 new_p2;)



let play_game length =
  let board = Array.make_matrix length length " + " in
  let (player1:player) = 
    {id = 1; games_won= 0;is_turn = true; color = " B "} in
  let (player2:player) = 
    {id = 2; games_won = 0; is_turn = false; color = " W "} in
  print_endline "These are how the coordinates work: ";
  print_coordinates length;
  move board player1 player2






(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our 3110 Final Project. Type 
                  quit to exit the game anytime. \n");
  let rec get_length () = 
    print_endline "Please enter the length of the board you want (13 or 15) \n";
    print_string  "> ";
    try let length = int_of_string (read_line()) in
      if length <> 13 && length <> 15 then 
        get_length ()
      else play_game length
    with Failure _ -> 
      print_endline "Bye have a beautiful time";
      exit 0 in
  get_length ()


(* Execute the game engine. *)
let () = main ()
