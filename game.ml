type board = string array array

let print_board (board: board) = 
  let print_line (line: string array) =  
    Array.fold_left (fun acc x -> acc ^ x) "" line in
  Array.iter(fun x -> print_endline (print_line x)) board



