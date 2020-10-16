(* TODO: Create helper functions for testing *)
open OUnit2

let firsttest name = 
  name >:: (fun  _-> assert_equal 2 3)


let f = [
  firsttest "hi";
]

let suite = 
  "test suite for A2"  >::: List.flatten [
    f
  ]


let _ = run_test_tt_main suite