
(* TODO: Create helper functions for testing *)
open OUnit2
open Game
open Bot
open ANSITerminal

let empty = [|[|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
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
                " + "; " + "; " + "; " + "; " + "|];
              [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                " + "; " + "; " + "; " + "; " + "|];
              [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                " + "; " + "; " + "; " + "; " + "|];
              [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                " + "; " + "; " + "; " + "; " + "|];
              [|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
                " + "; " + "; " + "; " + "; " + "|]|]

let reseted = [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|];
                [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                  " - "; " - "; " - "; " - "; " - "|]|]

let no_board = [||]

let tie_board = [|[|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|];
                  [|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
                    " B "; " B "; " B "; " B "; " B "|]|]

let test_player = {id = "clarkson"; games_won= 5; is_turn = true; color = " B "; 
                   last_move = [-1; -1]}
let test_player2 = {id = ""; games_won= 0; is_turn = false; color = " B "; 
                    last_move = [-1; -1]}

let test_player_won_game = {test_player with games_won = 6}
let test_player2_won_game = {test_player2 with games_won = 1}

let color_kwords = 
  [("red", [red]); ("magenta", [magenta]); ("yellow", [yellow]); 
   ("green", [green]); ("blue", [blue]); ("black", [black])]

let color_kwords_no_red = 
  [("magenta", [magenta]); ("yellow", [yellow]); 
   ("green", [green]); ("blue", [blue]); ("black", [black])]

let test_copy_board b = Array.map Array.copy b


let test_update_board b x y value =  
  let line = Array.get b (y - 1) in
  Array.set line (x - 1) value;
  Array.set b (y - 1) line

let board_1 = test_copy_board reseted 
let () = test_update_board board_1 15 15 " B "

let board_2 = test_copy_board empty 
let () = test_update_board board_2 14 15 " B " 
let () = test_update_board board_2 15 15 " B "

let row_2 = test_copy_board empty 
let () = test_update_board row_2 14 15 " B " 
let () = test_update_board row_2 15 15 " B "

let row_3 = test_copy_board row_2 
let () = test_update_board row_3 13 15 " B " 

let row_4 = test_copy_board row_3 
let () = test_update_board row_4 12 15 " B " 

let not_row_5 = test_copy_board row_4 
let () = test_update_board not_row_5 11 15 " W "

let not_row_5_2 = test_copy_board row_2 
let () = test_update_board not_row_5_2 13 15 " W " 
let () = test_update_board not_row_5_2 12 15 " W " 
let () = test_update_board not_row_5_2 11 15 " W " 

let not_row_5_3 = test_copy_board empty 
let () = test_update_board not_row_5_3 15 15 " W "
let () = test_update_board not_row_5_3 14 15 " B " 
let () = test_update_board not_row_5_3 13 15 " B " 
let () = test_update_board not_row_5_3 12 15 " B " 
let () = test_update_board not_row_5_3 11 15 " B " 

(* let board_4 = test_copy_board board_3 
   let () = test_update_board board_4 12 15 " B "  *)

let board_winner = test_copy_board empty
let () = test_update_board board_winner 15 15 " B "
let () = test_update_board board_winner 11 15 " B " 
let () = test_update_board board_winner 12 15 " B " 
let () = test_update_board board_winner 13 15 " B "  
let () = test_update_board board_winner 14 15 " B " 


(* let () = make_move board_2 13 15 " B "
   let () = make_move board_2 12 15 " B "
   let () = make_move board_2 11 15 " B " *)

(*; print_endline "c_board before reset:";
  print_color c_board
  let () = reset_board c_board
  let () = print_endline "c_board after reset:";
  print_color c_board*)

let equal_test name b1 b2 expected_output=
  print_endline "============================================";
  print_endline (name ^ " b1=");
  print_color b1; 
  print_endline (name ^ " b2=");
  print_color b2;
  name >:: (fun  _-> 
      assert_equal expected_output (b1 = b2)) 

let check_victor_test name board x y expected_output= 
  print_endline "============================================";
  print_endline name;
  print_color board;
  name >:: (fun  _-> assert_equal expected_output (check_victor board x y) )

let check_tie_test name board expected_output= 
  name >:: (fun  _-> assert_equal expected_output (check_tie board) )

let create_board_test name dimensions expected_output= 
  name >:: (fun  _-> assert_equal expected_output (create_board dimensions) )

let find_color_test name color expected_output= 
  name >:: (fun  _-> assert_equal expected_output (find_color color) )

let get_id_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output (get_id player) )

let get_turn_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output (get_turn player) )

let available_colors_test name color1 color_kwords acc expected_output= 
  name >:: (fun  _-> assert_equal expected_output 
               (available_colors color1 color_kwords acc) )

let update_score_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output 
               (update_score player) )

let reset_board_test name b1 b2 expected_output =  
  print_endline "============================================";
  print_endline (name ^ " before reset=");
  print_color b1;
  reset_board b1;
  print_endline (name ^ " after reset:");
  print_color b1;
  name >:: (fun  _-> 
      assert_equal expected_output (b1 = b2)) 

let bbot0  = [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                 " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|]|]

let bbot1  = [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                 " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " M "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " R "; " M "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" M "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " M "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " R "; " M "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " R "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|]|]

let bbot2  = [|[|" - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " Y "; " - ";
                 " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " Y "; " Y "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " R "; " - "; " - "; " - "; " Y "; " - "; " - "; " Y "; " - ";
                " - "; " - "; " - "|];
              [|" R "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" Y "; " R "; " R "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " R "; " - "; " Y "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|]|]

let bbot3  = [|[|" B "; " - "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                 " - "; " - "; " - "|];
              [|" R "; " B "; " R "; " - "; " - "; " R "; " - "; " - "; " - "; " - "; 
                " - "; " - "; " - "|];
              [|" - "; " R "; " R "; " - "; " B "; " - "; " - "; " - "; " - "; " - "; 
                " - "; " - "; " - "|];
              [|" - "; " - "; " R "; " B "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " B "; " R "; " B "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " R "; " - "; " - "; " R "; " B "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" B "; " - "; " B "; " - "; " B "; " B "; " R "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|];
              [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
                " - "; " - "; " - "|]|]

let rec step_with_bot name board player bot lst = 
  match lst with
  | [] -> (
    print_endline (name ^ " b1="); 
    print_color board;
    (List.nth player.last_move 0, List.nth player.last_move 1)
  )
  | (x, y) :: tl -> ( 
    if Array.get (Array.get board (y - 1)) (x - 1) <> " - " then (
      print_string [red] ("Bot took this spot already: (" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")");
      print_endline "";
      step_with_bot name board player bot tl
    )
    else (
      Game.make_move board x y player;
      let p = {player with last_move = [x;y]} in 
    if Game.check_victor board (y - 1) (x - 1) = true then (
          print_string [cyan] "PLAYER WON !";
          print_endline "";
          print_endline (name ^ " b1="); 
          print_color board;
          (x, y)
    )
    else (
      let p = Game.change_turn p in
      let b = Game.change_turn bot in
      match Bot.get_optimal_move board p b with
      | (x', y') -> ( 
        Game.make_move board (y' + 1) (x' + 1) bot;
        let b' = {b with last_move = [x';y']} in
        if Game.check_victor board x' y' = true then (
          print_string [cyan] "BOT WON !";
          print_endline "";
          print_endline (name ^ " b1="); 
          print_color board;
          (x', y')
        )
        else (
          let b' = Game.change_turn b' in
          let p'= Game.change_turn p in
          step_with_bot name board p' b' tl
        )
      )
    )
    )
   )

let test_bot_game name steps b2 expected_output =
  print_endline "======================================";
  let length = Array.length (Array.get b2 0) in 
  let board = Array.make_matrix length length " - " in
  let player = {id="TESTER";games_won = 0;is_turn = true;color=" B "; 
                last_move = [-1;-1]} in
  let bot = {id="BOT";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  ignore (step_with_bot name board player bot steps);
  print_endline (name ^ " b2=");
  print_color b2;
  name >:: (fun  _-> 
      assert_equal expected_output (board = b2)) 

let test_bot_game1 name expected_output =
  print_endline "======================================";
  let board = Array.make_matrix 13 13 " - " in
  let player = {id="P1";games_won = 0;is_turn = true;color=" M "; 
                last_move = [-1;-1]} in
  let bot = {id="BOT1";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  let steps = [(5, 4); (8, 7); (9, 3); (2, 6); (1, 5); (9, 9)] in
  ignore(step_with_bot name board player bot steps);
  print_endline (name ^ " b2=");
  print_color bbot1;
  name >:: (fun  _-> 
      assert_equal expected_output (board = bbot1)) 

let test_bot_game2 name expected_output =
  print_endline "======================================";
  let board = Array.make_matrix 13 13 " - " in
  let player = {id="P2";games_won = 0;is_turn = true;color=" Y "; 
                last_move = [-1;-1]} in
  let bot = {id="BOT2";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  let steps = [(5, 9); (4, 8); (1, 7); (6, 4); (2, 3); (9, 1); (3, 3); (4, 1); (9, 4); (11, 9)] in
  ignore (step_with_bot name board player bot steps);
  print_endline (name ^ " b2=");
  print_color bbot2;
  name >:: (fun  _-> 
      assert_equal expected_output (board = bbot2)) 

let test_bot_winner name steps expected_output =
  print_endline "======================================";
  let board = Array.make_matrix 13 13 " - " in
  let player = {id="TESTER";games_won = 0;is_turn = true;color=" G "; 
                last_move = [-1;-1]} in
  let bot = {id="BOT";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  let (x, y) = step_with_bot name board player bot steps in
  name >:: (fun  _-> assert_equal expected_output (check_victor board x y))


let board_tests = [
  equal_test "test not equal" board_1 empty false;
  check_victor_test "victor on winner board" board_winner 14 10 true;
  check_victor_test "victor on 4 in a row" row_4 14 11 false;
  check_victor_test "victor on 3 in a row" row_3 14 12 false;
  check_victor_test "victor on 2 in a row" row_2 14 14 false;
  check_victor_test "5 in a row, different last piece" not_row_5 14 10 false;
  check_victor_test "5 in a row, different pieces" not_row_5_2 14 10 false;
  check_victor_test "5 in a row, different first piece" not_row_5_3 14 10 false;
  check_victor_test "victor on 1 in a row" board_1 14 14 false;
  check_victor_test "victor on empty board" board_1 14 14 false;
  reset_board_test "test reset board" board_2 reseted true;
  check_tie_test "tie on empty board" reseted false;
  check_tie_test "tie on non-tie board" board_1 false;
  check_tie_test "tie on tie board" tie_board true;
  create_board_test "create no board" 0 no_board;
  create_board_test "create board of length 15" 15 reseted;
  find_color_test "find color that doesn't exist" "gold" " W ";
  find_color_test "find uppercase (should not exist)" "Black" " W ";
  find_color_test "find black" "black" " X ";
  find_color_test "find magenta" "magenta" " M ";
  find_color_test "find blue" "blue" " B ";
  find_color_test "find red" "red" " R ";
  find_color_test "find green" "green" " G ";
  find_color_test "find yellow" "yellow" " Y ";
  available_colors_test "test if player1 color is red" "red" color_kwords [] 
    color_kwords_no_red;
  get_id_test "get id of test_player" test_player "clarkson";
  get_turn_test "get turn of test_player" test_player true;
  get_id_test "get empty id of test_player2" test_player2 "";
  get_turn_test "get turn of test_player2" test_player2 false;
  update_score_test "update score for player with wins" 
    test_player test_player_won_game;
  update_score_test "update score for player with no wins" 
    test_player2 test_player2_won_game;
  test_bot_game1 "bot game 1 - comparing boards" true;
  test_bot_game2 "bot game 2 - comparing boards" true;
  test_bot_game "bot game 3 - comparing boards" 
    [(1, 1); (2, 2); (3, 3); (4, 4); (5, 5); 
     (6, 6); (3, 5); (5, 3); (1, 7); (5, 7);
     (3, 7); (6, 7); (8, 8); (9, 9); (10, 10);
     (11, 11); (12, 12); (13, 13)] bbot3 true;
  test_bot_winner "bot game 4 - bot won" 
    [(1, 3); (2, 9); (5, 3); (10, 11); (9, 5); 
     (10, 6); (7, 1)] true; 
  test_bot_winner "bot game 5 - player won" 
    [(1, 1); (2, 2); (3, 2); (4, 4); (5, 5); 
     (4, 6); (1, 5); (5, 3); (2, 7); (7, 2);
     (3, 7); (9, 5); (9, 6); (6, 5); (9, 7);
     (7, 5); (9, 8); (7, 3); (8, 4); (4, 1);
     (2, 6); (7, 6); (6, 6); (2, 5); (5, 6); 
     (6, 7); (5, 7); (9, 3)] true;
]

let suite = 
  "test suite for Gomoku"  >::: List.flatten [
    board_tests
  ]

let _ = run_test_tt_main suite

