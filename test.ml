(* TODO: Create helper functions for testing *)
open OUnit2
open Game





let firsttest name board expected_output= 
  name >:: (fun  _-> assert_equal 2 expected_output)


let f = [
  firsttest "hi";
]

let suite = 
  "test suite for A2"  >::: List.flatten [
    f
  ]


let _ = run_test_tt_main suite