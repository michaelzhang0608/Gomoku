open Sys
open Game
open ANSITerminal
open Go
open Random

let rec play_again board p1 p2 =  
  print_endline "Would you like to play again? (Y/N)";
  print_string [white] "> ";
  match read_line () with
  | command -> 
    if command <> "Y" && command <> "N" && command <> "quit" then 
      let p1 = {p1 with last_move = [-1;-1]} in
      let p2 = {p2 with last_move = [-1;-1]} in
      play_again board p1 p2
    else if command = "Y" then true
    else false

let victory board p1 (p2: Game.player) winner= 
  Game.print_color board;
  print_endline ((Game.get_id winner) ^ " has won!");
  if p1 = winner then begin
    print_endline "Score";
    print_endline (p1.id ^ ": " ^ (string_of_int (p1.games_won + 1)));
    print_endline (p2.id ^ ": " ^ (string_of_int p2.games_won));
    play_again (clear_board board) p1 p2 end
  else begin
    print_endline "Score";
    print_endline (p2.id ^ ": " ^ (string_of_int (p2.games_won + 1)));
    print_endline (p1.id ^ ": " ^ (string_of_int p1.games_won));
    play_again (clear_board board) p1 p2 end

let tie board p1 p2 = 
  Game.print_color board;
  print_endline ("Tie! You are both too smart.");
  play_again (clear_board board) p1 p2

let print_coordinates length = 
  for index = 0 to (length - 1) do 
    print_string [white] (" " ^ string_of_int (index) ^ " ");
  done;
  print_endline "";
  for index = 1 to (length - 1) do
    print_endline (string_of_int index);
    print_endline "";
  done


let rec get_x_coordinate length = 
  print_endline "Please enter the horizontal coordinate you want
    to place your piece in";
  print_string [white] "> ";
  try 
    let x = read_line() in
    if x = "quit" then begin 
      print_endline "Bye have a beautiful time";
      exit 0; end
    else if x = "save" then -1
    else 
      let x = Stdlib.int_of_string (x) in
      if x < 0 || x > (length - 1) then 
        get_x_coordinate length
      else x
  with Failure _ -> 
    print_string [red] "Invalid input, try again.";
    print_endline "";
    print_string [white] "> ";
    get_x_coordinate length

let rec get_y_coordinate length = 
  print_endline "Please enter the vertical coordinate you want
    to place your piece in";
  print_string [white] "> ";
  try 
    let y = read_line () in
    if y = "quit" then begin
      print_endline "Bye have a beautiful time";
      exit 0; end
    else if y = "save" then 
      -1
    else 
      let y = Stdlib.int_of_string (y) in
      if y < 0 || y > (length - 1) then 
        get_x_coordinate length
      else y
  with Failure _ -> 
    print_string [red] "Invalid input, try again.";
    print_endline "";
    print_string [white] "> ";
    get_y_coordinate length


let color_kwords = 
  [("red", [red]); ("magenta", [magenta]); ("yellow", [yellow]); 
   ("green", [green]); ("blue", [blue]); ("black", [black])]

let colors = ["red";"magenta"; "yellow";"green"; "blue"; "black"]

let print_color_command player lst= 
  if player = 0 then 
    print_endline ("Choose the color of your stone: \n")
  else 
    print_endline 
      ("Player " ^ string_of_int player ^ ", 
      choose the color of your stone: \n");
  for index = 0  to List.length lst - 1 do 
    print_string (snd (List.nth lst index)) (fst (List.nth lst index) ^ " ")
  done;
  print_string [white] " \n\n";
  print_string [white] ">"


let rec get_color () = 
  print_color_command 1 color_kwords;
  let color1 = String.lowercase_ascii (read_line ()) in
  if color1 = "quit" then exit 0;
  if List.mem color1 colors = false then begin
    print_string [red] "Invalid color try again"; 
    print_endline "";
    get_color (); end
  else  
    let colors2 = Game.available_colors color1 color_kwords [] in
    print_color_command 2 (colors2);
    let color2 = String.lowercase_ascii (read_line ()) in
    if color2 = "quit" then exit 0;
    if List.mem color2 colors = false then begin
      print_string [red] "Invalid color try again"; 
      print_endline "";
      get_color (); end
    else
      let color_list = [color1; color2] in
      let rec convert_color lst = match lst with
        | [] -> []
        | h :: t -> Game.find_color h :: convert_color t in
      convert_color color_list

let rec get_bot_color () = 
  print_color_command 0 color_kwords;
  let color = read_line() in
  if color = "quit" then exit 0;
  if List.mem color colors = false then begin
    print_string [red] "Invalid color try again"; 
    print_endline "";
    get_color (); end
  else begin
    let rec get_bot_color () = 
      let color_index = Random.int 6 in
      let bot_color = List.nth colors color_index in
      if bot_color = color then get_bot_color ()
      else begin
        let color_list = [color;bot_color] in
        let rec convert_color lst = match lst with
          | [] -> []
          | h :: t -> find_color h :: convert_color t in
        convert_color color_list end in
    get_bot_color () end



let get_names () = 
  print_endline "Enter the name of player 1";
  let player1 = read_line() in
  if player1 = "quit" then exit 0;
  print_endline "Enter the name of player 2";
  let player2 = read_line() in
  if player2 = "quit" then exit 0;
  [player1;player2]


let get_name () = 
  print_endline "Enter the name of your player";
  let human = read_line () in
  if human = "quit" then exit 0;
  human


let save_game (board : string array array) = 
  let fold_function acc lst = 
    acc @ [(Array.to_list lst)] in
  let lst = Array.fold_left fold_function [] board in
  Csv.save "board.csv" lst

let save_human_players (player1: Game.player) (player2 : Game.player) = 
  let lst1 = [player1.id;string_of_int(player1.games_won);
              if player1.is_turn then "true" else "false";player1.color;
              string_of_int (List.nth (player1.last_move) 0);
              string_of_int (List.nth (player1.last_move )1)] in
  let lst2 = [player2.id;string_of_int(player2.games_won);
              if player2.is_turn then "true" else "false";player2.color;
              string_of_int (List.nth (player2.last_move) 0);
              string_of_int (List.nth (player2.last_move )1)] in
  Csv.save "players.csv" ([lst1] @ [lst2])

let save_bot_players (player: Game.player) (bot: Game.player) bool difficulty = 
  let lst1 = [player.id;string_of_int(player.games_won);
              if player.is_turn then "true" else "false";player.color;
              string_of_int (List.nth (player.last_move) 0);
              string_of_int (List.nth (player.last_move )1); 
              if bool then "true" else "false";
              difficulty] in
  let lst2 = [bot.id;string_of_int(player.games_won);
              if bot.is_turn then "true" else "false";bot.color;
              string_of_int (List.nth (bot.last_move) 0);
              string_of_int (List.nth (bot.last_move )1); 
              if bool then "true" else "false";
              difficulty] in
  Csv.save "players.csv" ([lst1] @ [lst2])



let rec move (board : string array array) (p1: Game.player) (p2:Game.player) = 
  Game.print_color board;
  let x = get_x_coordinate (Array.length board) in
  if x = -1 then begin
    print_endline "Bye";
    save_human_players p1 p2;
    save_game board;
    exit 0; end
  else let y = get_y_coordinate (Array.length board) in
    if Game.get_turn p1 then begin
      if Array.get (Array.get board (y - 1)) (x - 1) <> " - " then  begin
        print_string [red] "Invalid move";
        print_endline "";
        move board p1 p2 end
      else 
        Game.make_move board x y p1;
      let p1 = {p1 with last_move = [x;y]} in 
      if Game.check_tie board then
        (if tie board p1 p2 then 
           move (Game.clear_board board) p1 p2
         else print_endline "Bye have a beautiful time"; exit 0; ) 
      else if Game.check_victor board (y - 1) (x - 1)= true then 
        (if victory board p1 p2 p1 then 
           let new_p1 = Game.update_games_won p1 in 
           move (Game.clear_board board) new_p1 p2;
         else print_endline "Bye have a beautiful time"; exit 0; )
      else 
        let new_p1 = Game.change_turn p1 in
        let new_p2 = Game.change_turn p2 in
        move board new_p1 new_p2; end
    else 
      let p2 = {p2 with last_move = [x;y]} in
      if Array.get (Array.get board (y - 1)) (x - 1) <> " - " then  begin
        print_string [red] "Invalid move";
        print_endline "";
        move board p1 p2 end
      else 
        Game.make_move board x y p2;
      if Game.check_tie board then
        (if tie board p1 p2 then 
           move (Game.clear_board board) p1 p2
         else print_endline "Bye have a beautiful time"; exit 0; ) 
      else
      if Game.check_victor board (y - 1) (x - 1)= true then
        (if victory board p1 p2 p2 then 
           let new_p2 = Game.update_games_won p2 in
           move (Game.clear_board board) p1 new_p2;
         else print_endline "Bye have a beautiful time"; exit 0;)
      else 
        let new_p1 = Game.change_turn p1 in
        let new_p2 = Game.change_turn p2 in
        move board new_p1 new_p2 




let rec play_with_bot board player bot bool who_goes_first difficulty= 
  if bool then begin
    Game.print_color board;
    let x = get_x_coordinate (Array.length board) in
    if x = -1 then begin
      print_endline "Bye";
      save_bot_players player bot who_goes_first difficulty;
      save_game board;
      exit 0; end
    else 
      let y = get_y_coordinate (Array.length board) in
      if Array.get (Array.get board (y - 1)) (x - 1) <> " - " then  begin
        print_string [red] "Invalid move";
        print_endline "";
        play_with_bot board player bot bool who_goes_first difficulty end
      else 
        Game.make_move board x y player;
      let new_player = {player with last_move = [x;y]} in 
      if Game.check_tie board then begin
        if tie board new_player bot then 
          play_with_bot (clear_board board) new_player bot bool who_goes_first
            difficulty
        else print_endline "Bye have a beautiful time"; exit 0; end
      else if Game.check_victor board (y - 1) (x - 1)= true then begin
        if victory board new_player bot new_player then 
          let new_player = Game.update_games_won new_player in 
          play_with_bot(clear_board board) 
            new_player bot who_goes_first who_goes_first difficulty;
        else print_endline "Bye have a beautiful time"; exit 0; end 
      else 
        let new_player = Game.change_turn new_player in
        let bot = Game.change_turn bot in
        play_with_bot board new_player bot new_player.is_turn who_goes_first 
          difficulty end
  else begin
    match Bot.get_optimal_move board player bot difficulty with
    | (x,y) -> 
      let new_bot = {bot with last_move = [x;y]} in
      Game.make_move board (y + 1) (x + 1) bot;
      if Game.check_tie board then begin
        if tie board player new_bot then
          play_with_bot (clear_board board) player new_bot bool who_goes_first 
            difficulty
        else print_endline "Bye have a beautiful time"; exit 0; end
      else 
      if Game.check_victor board x y = true then begin
        if victory board new_bot player new_bot then
          let new_bot = Game.update_games_won new_bot in
          let new_bot = {new_bot with last_move = [-1;-1]} in
          play_with_bot (clear_board board ) player new_bot 
            who_goes_first who_goes_first difficulty;
        else print_endline "Bye have a beautiful time"; exit 0;  end
      else  
        let new_bot = Game.change_turn new_bot in
        let player= Game.change_turn player in
        play_with_bot board player new_bot player.is_turn who_goes_first 
          difficulty end



let rec difficulty_level () = 
  print_endline "Enter the bot difficulty: easy, medium, or hard ";
  let difficulty = read_line () in
  if difficulty = "easy" || difficulty = "medium" || difficulty = "hard"
  then difficulty
  else difficulty_level ()


let rec bot_game length =
  let difficulty = difficulty_level () in
  let color_list = get_bot_color () in
  let human_color = List.nth color_list 0 in
  let bot_color = List.nth color_list 1 in
  let name = get_name () in
  let rec get_turn () = 
    print_endline "Would you like to go first? (Y/N) ";
    match read_line() with
    | "Y" ->  
      let player = {id=name;games_won = 0;is_turn = true;color=human_color; 
                    last_move = [-1;-1]} in
      let bot = {id="Tarzan";games_won = 0;is_turn =false;color=bot_color;
                 last_move=[-1;-1]} in 
      play_with_bot (Game.create_board length) player bot player.is_turn true 
        difficulty
    | "N" -> 
      let player = {id=name;games_won = 0;is_turn = false;color=human_color; 
                    last_move = [-1;-1]} in
      let bot = {id="Tarzan";games_won = 0;is_turn =true;color=bot_color;
                 last_move=[-1;-1]} in
      play_with_bot (Game.create_board length) player bot player.is_turn false 
        difficulty
    | "quit" -> exit 0
    | _->  get_turn () in get_turn ()



let play_game length =
  let board = Array.make_matrix length length " - " in
  let color_list = get_color () in
  let color1 = List.nth color_list 0 in
  let color2 = List.nth color_list 1 in
  let names = get_names() in
  let (player1:Game.player) = 
    {id = (List.nth names 0); games_won= 0;is_turn = true; color = color1; 
     last_move = [-1; -1]} in
  let (player2:Game.player) = 
    {id = (List.nth names 1); games_won = 0; is_turn = false; color = color2; 
     last_move = [-1; -1]} in
  print_endline "These are how the coordinates work: ";
  print_coordinates length;
  move board player1 player2



let load_game name = 
  let lst = Csv.load name in
  let lst_to_board lst = 
    Array.of_list (List.map Array.of_list lst) in
  lst_to_board lst

let load_players name = 
  let lst = Csv.load name in
  let player1_data = List.nth lst 0 in
  let player1 = {id = List.nth player1_data 0; 
                 games_won = int_of_string (List.nth player1_data 1); 
                 is_turn = bool_of_string(List.nth player1_data 2);
                 color = List.nth player1_data 3; 
                 last_move = [int_of_string(List.nth player1_data 4); 
                              int_of_string(List.nth player1_data 5)]} in
  let player2_data = List.nth lst 1 in
  let player2 = {id = List.nth player2_data 0; 
                 games_won = int_of_string (List.nth player2_data 1); 
                 is_turn = bool_of_string (List.nth player2_data 2);
                 color = List.nth player2_data 3; 
                 last_move = [int_of_string(List.nth player2_data 4); 
                              int_of_string(List.nth player2_data 5)]} in
  if bool_of_string (List.nth player2_data 6) then 
    (player1, false, player2, true, true, List.nth player2_data 7)
  else (player1, false, player2, true, false, List.nth player2_data 7) 



let rec get_length () = 
  print_endline "Please enter the length of the board you want (13 or 15) \n";
  print_string [white] "> ";
  try let length = int_of_string (read_line()) in
    if length <> 13 && length <> 15 then 
      get_length ()
    else begin
      let rec choice () = 
        print_endline "Will you be playing against our bot? (Y/N) ";
        let ans  = read_line () in
        if ans = "Y" then bot_game length
        else if ans = "N" then play_game length
        else if ans = "quit" then exit 0
        else choice () in
      choice () end
  with Failure _ -> 
    print_endline "Bye have a beautiful time";
    exit 0 

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red] "\n\nWelcome. Type quit to exit the game 
  anytime, type save to save the current game.\n");
  let rec load () = 
    print_endline "Do you want to load a previous game? (Y/N)";
    let ans = read_line () in
    if ans = "Y" then begin
      print_endline "Enter the file name for the board";
      let filename = read_line () in
      try let board = load_game filename in 
        print_endline "Enter the file name for the players";
        try let players = read_line () in
          match load_players players with
          |(one, false, two, false, _, _) -> move board one two
          | (one, false, two, true, true, difficulty) -> 
            play_with_bot board one two one.is_turn true difficulty
          | (one, false, two, true, false, difficulty) -> 
            play_with_bot board one two one.is_turn false difficulty
        with Sys_error _ -> load () 
      with Sys_error _ -> load ()
    end
    else if ans = "N" then get_length () 
    else load () in load ()

let print_board board = 
  let print_line (line: string array) =  
    Array.fold_left (fun acc x -> acc ^ x) "" line in
  Array.iter(fun x -> print_endline (print_line x)) board



(* Execute the game engine. *)
let () = main ()

