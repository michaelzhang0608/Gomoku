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

let board_winner = test_copy_board empty
let () = test_update_board board_winner 11 15 " B " 
let () = test_update_board board_winner 12 15 " B " 
let () = test_update_board board_winner 13 15 " B "  
let () = test_update_board board_winner 14 15 " B " 
let () = test_update_board board_winner 15 15 " B "

let () = make_move board_2 13 15 " B "
let () = make_move board_2 12 15 " B "
let () = make_move board_2 11 15 " B "(*;
                                        print_endline "c_board before reset:";
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
  name >:: (fun  _-> assert_equal expected_output (check_victor board x y) )

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
  check_victor_test "victor on winner board" board_winner 14 10 "true";
  check_victor_test "victor on empty board" board_1 14 14 "false";
  reset_board_test "test reset board" board_2 empty true;
]



let suite = 
  "test suite for Gomoku"  >::: List.flatten [
    board_tests
  ]

let _ = run_test_tt_main suite