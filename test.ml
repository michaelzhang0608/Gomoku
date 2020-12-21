open OUnit2
open Game
open Bot
open ANSITerminal

(** 
   Representation of the bot player for Gomoku.

   This module represents the bot and functions that result in bot moves of
   varying difficulty.

   TESTING PLAN FOR GAME MODULE

   We manually tested the functions in Main.ml. Many required user inputs and 
   reading the inputed values, such the get x coordinate or y coordinate 
   functions, and functions for interpreting the ID and color of the player’s 
   stone. These functions were tested manually through playing the game. This 
   form of testing measured the correctness of the system because we could 
   visually see if the system responded correctly to the user inputs and/or 
   provided the correct feedback to an invalid input.

   In the Game module, we used OUnit to the functions that helped to keep track 
   of the state of the game, and determined the winner. This includes the 
   functions check_victor, check_tie, create_board, find_color, get_id, 
   get_turn, get_games_won, change_turn, available_colors, update_score, and 
   clear_board. These functions determine the correctness of the program by 
   calculating the necessary booleans and values for the game state to progress. 
   Through our testing, we checked the accuracy of each function. For example, 
   we checked if the check_winner function would return true on a board with 
   five stones in a row versus if it would return false with four stones in a 
   row. Test cases were developed through black-box testing such that we could 
   practice test-driven development. We also utilized helper functions for 
   testing to increase efficiency. 

   Functions related to saving the game board or game players were tested 
   manually with CSV files, because it would override the original CSV file 
   each time the function is applied. The make_move function was tested by 
   testing if the resulting board matched a different board. 


   TESTING PLAN FOR BOT MODULE

   We did Ounit testing for the Bot module. 

   In order to properly test Bot, we first have to ensure that the bot makes 
   the best optimal move. We manually calculated the expected optimal move bot
   makes through the heuristic algorithm––weights are assigned to the vicinty
   of Player's last move and the empty spot with the most weight is the optimal
   move. We compare the expected optimal move with the different difficulties of
   bot on different boards. 

   Next, we check to see if the bot is actually capable of making a move on its
   own based on a list of Player's move. The idea is to simulate game play in 
   which a the bot continues to make its own move based on the each of Player's
   moves. We check to see if the Bot actually does this correctly by comparing
   the board that the Player list and Bot populates to a board we make for 
   comparison. Since the test always passes, we know that Bot is correctly
   populating the board because it is populating the board in the same way
   every time based on the Player's list of moves. We do this for the different
   difficulties of bot. This logic is similar from that of main.

   Finally, we check to see if we can properly compute the victor of the game
   when playing with bot and its various difficulties. We puposefully play
   a game that loses and a game that wins against bot's various difficulties. I
   could not win aginst the hard mode of bot no matter how hard I tried, showing
   that this AI is quite optimal. Ultimately, we used black-box testing to 
   determine the result of the game rather than looking specifically at the 
   implementation of the bot for testing purposes. 
*)

(* Boards to check our testing results against *)
let empty = 
  [|[|" + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + "; " + ";
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

let reseted = 
  [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
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

let tie_board = 
  [|[|" B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B "; " B ";
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

(* Test players to check our testing results against *)
let test_player = {id = "clarkson"; games_won= 5; is_turn = true; color = " B "; 
                   last_move = [-1; -1]}
let test_player2 = {id = ""; games_won= 0; is_turn = false; color = " B "; 
                    last_move = [-1; -1]}


let test_player_won_game = {test_player with games_won = 6}
let test_player2_won_game = {test_player2 with games_won = 1}

let test_player_turn = {id = "clarkson"; games_won= 5; is_turn = false; 
                        color = " B "; last_move = [-1; -1]}
let test_player2_turn = {id = ""; games_won= 0; is_turn = true; color = " B "; 
                         last_move = [-1; -1]}

let load_player1 = {id = "r"; games_won= 1; is_turn = true; color = " R "; 
                    last_move = [1; 5]}

let load_player2 = {id = "t"; games_won= 0; is_turn = false; color = " B "; 
                    last_move = [5; 5]}


let color_kwords = 
  [("red", [red]); ("magenta", [magenta]); ("yellow", [yellow]); 
   ("green", [green]); ("blue", [blue]); ("black", [black])]

let color_kwords_no_red = 
  [("magenta", [magenta]); ("yellow", [yellow]); 
   ("green", [green]); ("blue", [blue]); ("black", [black])]

let test_copy_board b = Array.map Array.copy b

(* Creating specific scenarios to test against *)
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

let col_2 = test_copy_board empty 
let () = test_update_board col_2 9 9 " B " 
let () = test_update_board col_2 9 8 " B "

let col_3 = test_copy_board col_2 
let () = test_update_board col_3 9 7 " B " 

let col_4 = test_copy_board col_3 
let () = test_update_board row_4 9 10 " B " 

let not_col_5 = test_copy_board col_4 
let () = test_update_board not_col_5 11 11 " B "

let col_5 = test_copy_board not_col_5 
let () = test_update_board col_5 9 6 " B "

let nw_2 = test_copy_board empty 
let () = test_update_board nw_2 9 9 " B " 
let () = test_update_board nw_2 10 10 " B "

let nw_3 = test_copy_board nw_2 
let () = test_update_board nw_3 11 11 " B " 

let nw_4 = test_copy_board nw_3 
let () = test_update_board nw_4 8 8 " B " 

let not_nw_5 = test_copy_board nw_4 
let () = test_update_board not_nw_5 7 11 " B "

let nw_5 = test_copy_board not_nw_5 
let () = test_update_board nw_5 7 7 " B "

let board_winner = test_copy_board empty
let () = test_update_board board_winner 15 15 " B "
let () = test_update_board board_winner 11 15 " B " 
let () = test_update_board board_winner 12 15 " B " 
let () = test_update_board board_winner 13 15 " B "  
let () = test_update_board board_winner 14 15 " B " 

let empty_table = load_game "board.csv"
let saved_board = save_board board_winner "update.csv" 
let saved_winner = save_human_players test_player test_player2 "update.csv" 

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

let get_games_won_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output (get_games_won player) )

let change_turn_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output (change_turn player) )

let available_colors_test name color1 color_kwords acc expected_output= 
  name >:: (fun  _-> assert_equal expected_output 
               (available_colors color1 color_kwords acc) )

let update_score_test name player expected_output= 
  name >:: (fun  _-> assert_equal expected_output 
               (update_score player) )

let clear_board_test name board expected_output= 
  let () = clear_board board in
  name >:: (fun  _-> assert_equal expected_output  board )

let load_game_test name file_name expected_output = 
  name >:: (fun _ -> assert_equal expected_output (load_game "board.csv"))

let load_players_test name file_name expected_output = 
  name >:: (fun _ -> assert_equal expected_output 
               (load_players file_name))

let reset_board_test name b1 b2 expected_output =  
  print_endline "============================================";
  print_endline (name ^ " before reset=");
  print_color b1;
  clear_board b1;
  print_endline (name ^ " after reset:");
  print_color b1;
  name >:: (fun  _-> 
      assert_equal expected_output (b1 = b2)) 

let bbot  = 
  [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
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

let bbot0  = 
  [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " Y "; " R "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " Y "; " - "; " Y "; " - "; " - "; " - "; " - ";
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

let bbot1easy  = 
  [|[|" R "; " R "; " R "; " R "; " R "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " M "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " M "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" M "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " M "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " R "; " M "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " M "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|]|]

let bbot1  = 
  [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " R "; " - "; " - "; " - "; " - "; " - "; " M "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " R "; " M "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" M "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " M "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " R "; " M "; " - "; " - ";
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

let bbot1hard  = 
  [|[|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " M "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " M "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" M "; " - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " M "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " R "; " M "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
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

let bbot2easy  = 
  [|[|" R "; " R "; " R "; " R "; " R "; " - "; " - "; " - "; " Y "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " Y "; " Y "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " Y "; " - "; " - "; " Y "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" Y "; " - "; " R "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " - "; " - ";
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

let bbot2  = 
  [|[|" - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " Y "; " - ";
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

let bbot2hard  = 
  [|[|" - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " Y "; " - ";
      " - "; " - "; " - "|];
    [|" R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" R "; " Y "; " Y "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" R "; " - "; " - "; " - "; " - "; " Y "; " - "; " - "; " Y "; " - ";
      " - "; " - "; " - "|];
    [|" R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" R "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" Y "; " - "; " R "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " Y "; " - "; " - "; " - "; " - "; " - "; " - ";
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

let bbot3easy  = 
  [|[|" B "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " B "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " R "; " - "; " B "; " - "; " - "; " - "; " - "; " - "; 
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " B "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " B "; " - "; " B "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" B "; " - "; " - "; " - "; " - "; " - "; " R "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " R "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " R "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " R ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|];
    [|" - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
      " - "; " - "; " - "|]|]

let bbot3  = 
  [|[|" B "; " - "; " R "; " - "; " - "; " - "; " - "; " - "; " - "; " - ";
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

(* Given the testing board check to see if the bot puts the next move in the 
   most optimal place. The most optimal place is calculated throgh a ___ 
   algorithm that will always compute the same most optimal coordinate at a 
   certain (y,x). *)

let test_bot_optimal_move name board pcolor plast blast level expected_output =
  let player = {id="TESTER";games_won = 0;is_turn = true;color=pcolor; 
                last_move = plast} in
  let bot = {id="BOT";games_won = 0;is_turn =false;color=" R ";
             last_move=blast} in
  let p = Game.change_turn player in
  let b = Game.change_turn bot in
  let (y, x) = Bot.get_optimal_move board p b level in
  name >:: (fun  _-> 
      assert_equal expected_output (x + 1, y + 1)) 


(* Same logic as main but not the exactly the same. The caller defines the 
   player and bot. After the player makes a move, the bot makes a move. *)
let rec step_with_bot name board player bot level lst = 
  (* ends when there are no more player moves in the list to make or 
     someone wins *)
  match lst with
  | [] -> (
      print_endline (name ^ " b1="); 
      print_color board;
      (List.nth player.last_move 0, List.nth player.last_move 1)
    )
  | (x, y) :: tl -> ( 
      (* check if bot has taken the spot that player is about to but *)
      if Array.get (Array.get board (y - 1)) (x - 1) <> " - " then (
        print_string [red] 
          ("Bot took this spot already: (" ^ (string_of_int x) ^ 
           ", " ^ (string_of_int y) ^ ")");
        print_endline "";
        step_with_bot name board player bot level tl
      )
      (* player takes the spot and checks if player wins *)
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
          (* Player has already made the move. It is bot's turn. Player's move 
             did not win and does not conflict with bot's placement. Bot has to 
             make the optimal move.  *)
          let p = Game.change_turn p in
          let b = Game.change_turn bot in
          match Bot.get_optimal_move board p b level with
          | (x', y') -> ( 
              Game.make_move board (y' + 1) (x' + 1) bot;
              let b' = {b with last_move = [x';y']} in
              (* bot makes the move and checks if bot wins *)
              if Game.check_victor board x' y' = true then (
                print_string [cyan] "BOT WON !";
                print_endline "";
                print_endline (name ^ " b1="); 
                print_color board;
                (y' + 1, x' + 1)
              )
              (* bot doesn't win so it's player's turn *)
              else (
                let b' = Game.change_turn b' in
                let p'= Game.change_turn p in
                step_with_bot name board p' b' level tl
              )
            )
        )
      )
    )

(* did bot populate board *)
let test_bot_game name steps b2 color level expected_output =
  print_endline "======================================";
  let length = Array.length (Array.get b2 0) in 
  let board = Array.make_matrix length length " - " in
  let player = {id="TESTER";games_won = 0;is_turn = true;color=color; 
                last_move = [-1;-1]} in
  let bot = {id="BOT";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  (* start the bot game and ignore the output *)
  ignore (step_with_bot name board player bot level steps);
  print_endline (name ^ " b2=");
  print_color b2;
  name >:: (fun  _-> 
      assert_equal expected_output (board = b2)) 
(* test who is victor of populated board. print out to visually see that 
   it's right *)
let test_bot_winner name steps level expected_output =
  print_endline "======================================";
  let board = Array.make_matrix 13 13 " - " in
  let player = {id="TESTER";games_won = 0;is_turn = true;color=" G "; 
                last_move = [-1;-1]} in
  let bot = {id="BOT";games_won = 0;is_turn =false;color=" R ";
             last_move=[-1;-1]} in
  let (x, y) = step_with_bot name board player bot level steps in
  name >:: 
  (fun  _-> assert_equal expected_output (check_victor board (y - 1) (x - 1)))


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
  check_victor_test "not 5 in a col" not_col_5 10 10 false;
  check_victor_test "5 in northwest diagonal" nw_5 5 5 true;
  check_victor_test "not 5 in northwest diagonal" not_nw_5 10 6 false;
  reset_board_test "test reset board" board_2 reseted true;
  check_tie_test "tie on empty board" reseted false;
  check_tie_test "tie on non-tie board" board_1 false;
  check_tie_test "tie on tie board" tie_board true;
  create_board_test "create no board" 0 no_board;
  create_board_test "create board of length 15" 15 reseted;
  clear_board_test "clear no board" no_board no_board;
  clear_board_test "clear empty board " reseted reseted;
  clear_board_test "clear board of length 15" board_winner reseted;
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
  available_colors_test "if color list is empty" "" [] [] [];
  available_colors_test "if color not in list" "gold" color_kwords [] 
    color_kwords;
  get_id_test "get id of test_player" test_player "clarkson";
  get_turn_test "get turn of test_player" test_player true;
  get_games_won_test "get games won of test_player" test_player 5;
  change_turn_test "change turn for test_player2" test_player test_player_turn;
  get_id_test "get empty id of test_player2" test_player2 "";
  get_turn_test "get turn of test_player2" test_player2 false;
  change_turn_test "change turn for test_player2" test_player2 
    test_player2_turn;
  get_games_won_test "get games won of test_player2" test_player2 0;
  update_score_test "update score for player with wins" 
    test_player test_player_won_game;
  update_score_test "update score for player with no wins" 
    test_player2 test_player2_won_game;
  load_game_test "load board.csv file" "board.csv" reseted;
  load_game_test "load updated update.csv file" "update.csv" reseted;
  load_players_test "load players from updated update.csv file" 
    "players.csv" (load_player1, false, load_player2, false, true, "")
]

let bot_tests = 
  [test_bot_optimal_move "bot(easy) optimal move (6, 2)" 
     bbot0 " Y " [6;5] [6;3] "easy" (6, 2);
   test_bot_optimal_move "bot(medium) optimal move (6, 2)" 
     bbot0 " Y " [6;5] [6;3] "medium" (6, 2);
   test_bot_optimal_move "bot(hard) optimal move (6, 2)" 
     bbot0 " Y " [6;5] [6;3] "hard" (6, 2);
   test_bot_optimal_move "bot(easy) optimal move (2, 2)" 
     bbot1 " M " [9;3] [5;5] "easy" (2, 2);
   test_bot_optimal_move "bot(medium) optimal move (2, 2)" 
     bbot1 " M " [9;3] [5;5] "medium" (2, 2);
   test_bot_optimal_move "bot(hard) optimal move (8, 8)" 
     bbot1 " M " [9;3] [5;5] "hard" (8, 8);
   test_bot_optimal_move "bot(easy) optimal move (2, 9)" 
     bbot2 " Y " [11;9] [8;2] "easy" (2, 9);
   test_bot_optimal_move "bot(medium) optimal move (2, 9)" 
     bbot2 " Y " [11;9] [8;2] "medium" (2, 9);
   test_bot_optimal_move "bot(hard) optimal move (2, 9)" 
     bbot2 " Y " [11;9] [8;2] "hard" (2, 9);
   test_bot_optimal_move "bot(easy) optimal move (4, 3)" 
     bbot3 " B " [13;13] [1;3] "easy" (4, 3);
   test_bot_optimal_move "bot(medium) optimal move (4, 7)" 
     bbot3 " B " [13;13] [1;3] "medium" (4, 7);
   test_bot_optimal_move "bot(hard) optimal move (4, 7)" 
     bbot3 " B " [13;13] [1;3] "hard" (4, 7);
   test_bot_game "bot(easy) game 1 - comparing boards" 
     [(5, 4); (8, 7); (9, 3); (2, 6); (1, 5); (9, 9)]
     bbot1easy " M " "easy" true;
   test_bot_game "bot(medium) game 1 - comparing boards" 
     [(5, 4); (8, 7); (9, 3); (2, 6); (1, 5); (9, 9)]
     bbot1 " M " "medium" true;
   test_bot_game "bot(hard) game 1 - comparing boards" 
     [(5, 4); (8, 7); (9, 3); (2, 6); (1, 5); (9, 9)]
     bbot1hard " M " "hard" true;
   test_bot_game "bot(easy) game 2 - comparing boards" 
     [(5, 9); (4, 8); (1, 7); (6, 4); (2, 3); 
      (9, 1); (3, 3); (4, 1); (9, 4); (11, 9)]
     bbot2easy " Y " "easy" true;
   test_bot_game "bot(medium) game 2 - comparing boards" 
     [(5, 9); (4, 8); (1, 7); (6, 4); (2, 3); 
      (9, 1); (3, 3); (4, 1); (9, 4); (11, 9)]
     bbot2 " Y " "medium" true;
   test_bot_game "bot(hard) game 2 - comparing boards" 
     [(5, 9); (4, 8); (1, 7); (6, 4); (2, 3); 
      (9, 1); (3, 3); (4, 1); (9, 4); (11, 9)]
     bbot2hard " Y " "hard" true;
   test_bot_game "bot(easy) game 3 - comparing boards" 
     [(1, 1); (2, 2); (3, 3); (4, 4); (5, 5); 
      (6, 6); (3, 5); (5, 3); (1, 7); (5, 7);
      (3, 7); (6, 7); (8, 8); (9, 9); (10, 10);
      (11, 11); (12, 12); (13, 13)] bbot3easy " B " "easy" true;
   test_bot_game "bot(medium) game 3 - comparing boards" 
     [(1, 1); (2, 2); (3, 3); (4, 4); (5, 5); 
      (6, 6); (3, 5); (5, 3); (1, 7); (5, 7);
      (3, 7); (6, 7); (8, 8); (9, 9); (10, 10);
      (11, 11); (12, 12); (13, 13)] bbot3 " B " "medium" true;
   test_bot_game "bot(hard) game 3 - comparing boards" 
     [(1, 1); (2, 2); (3, 3); (4, 4); (5, 5); 
      (6, 6); (3, 5); (5, 3); (1, 7); (5, 7);
      (3, 7); (6, 7); (8, 8); (9, 9); (10, 10);
      (11, 11); (12, 12); (13, 13)] bbot3 " B " "hard" true;
   test_bot_winner "bot(easy) game 4 - bot won" 
     [(1, 3); (2, 9); (5, 3); (10, 11); (9, 5); 
      (10, 6); (7, 1)] "easy" true; 
   test_bot_winner "bot(medium) game 4 - bot won" 
     [(1, 3); (2, 9); (5, 3); (10, 11); (9, 5); 
      (10, 6); (7, 1)] "medium" true; 
   test_bot_winner "bot(hard) game 4 - bot won" 
     [(1, 3); (2, 9); (5, 3); (10, 11); (9, 5); 
      (10, 6); (7, 1)] "hard" true; 
   test_bot_winner "bot(easy) game 5 - player won" 
     [(1, 1); (2, 2); (3, 2); (4, 4); (5, 5); 
      (4, 6); (1, 5); (5, 3); (2, 7); (7, 2);
      (3, 7); (9, 5); (9, 6); (6, 5); (9, 7);
      (5, 6); (6, 7); (5, 7); (4, 7)] "easy" true;
   test_bot_winner "bot(medium) game 5 - player won" 
     [(1, 1); (2, 2); (3, 2); (4, 4); (5, 5); 
      (4, 6); (1, 5); (5, 3); (2, 7); (7, 2);
      (3, 7); (9, 5); (9, 6); (6, 5); (9, 7);
      (7, 5); (9, 8); (7, 3); (8, 4); (4, 1);
      (2, 6); (7, 6); (6, 6); (2, 5); (5, 6); 
      (6, 7); (5, 7); (9, 3)] "medium" true;
   test_bot_winner "bot(hard) game 5 - player failed to win" 
     [(1, 1); (2, 2); (3, 2); (4, 4); (5, 5); 
      (4, 6); (1, 5); (5, 3); (2, 7); (7, 2);
      (5, 6); (3, 7); (1, 9); (1, 7); (6, 4);
      (5, 9); (8, 6); (7, 6)] "hard" false;
  ]

let suite = 
  "test suite for Gomoku"  >::: List.flatten [
    board_tests;
    bot_tests
  ]

let _ = run_test_tt_main suite


