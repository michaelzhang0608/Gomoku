(* TODO: Create helper functions for testing *)
open OUnit2
open Game

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


let test_copy_board b = Array.map Array.copy b

let test_update_board b x y value =  
  let line = Array.get b (y - 1) in
  Array.set line (x - 1) value;
  Array.set b (y - 1) line

let board_1 = test_copy_board empty 
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
  print_endline (name ^ " b1=");
  print_color b1; 
  print_endline (name ^ " b2=");
  print_color b2;
  name >:: (fun  _-> 
      assert_equal expected_output (b1 = b2); 
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

let check_victor_test name board x y expected_output= 
  print_endline name;
  print_color board;
  name >:: (fun  _-> assert_equal expected_output (check_victor board x y) )

let check_tie_test name board expected_output= 
  name >:: (fun  _-> assert_equal expected_output (check_tie board) )

let reset_board_test name b1 b2 expected_output =  
  print_endline (name ^ " before reset=");
  print_color b1;
  reset_board b1;
  print_endline (name ^ " after reset:");
  print_color b1;
  name >:: (fun  _-> 
      assert_equal expected_output (b1 = b2); 
      print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")


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
  reset_board_test "test reset board" board_2 empty true;
  check_tie_test "tie on empty board" empty false;
  check_tie_test "tie on non-tie board" board_1 false;
  check_tie_test "tie on tie board" tie_board true;
]

let suite = 
  "test suite for Gomoku"  >::: List.flatten [
    board_tests
  ]

let _ = run_test_tt_main suite

