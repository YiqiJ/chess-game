(** [print_state st] is a string form of the current chess [lst]. *)
let print_state lst = 
  let rec helper lst = 
    match lst with 
    | [] -> ""
    | ((name, id), (a, b)) ::[] -> name^"-"^(string_of_int id)^" ("^
                                   (string_of_int a)^", "^(string_of_int b)^
                                   ")" 
    | ((name, id), (a, b)) ::t -> name^"-"^(string_of_int id)^" ("^
                                  (string_of_int a)^", "^(string_of_int b)^
                                  ") ; "^helper t 
  in 
  helper lst

(** [row_chess_sort r lst] is a sorted row based on the column number. 
    Sorted from the smallest column number to the largest column number. *)
let row_chess_sort r lst =  
  let rec helper = function
    | [] -> []
    | ((name, id), (a, b)) as e ::t -> if b = r then e :: helper t 
      else helper t
  in 
  helper lst |> List.sort (fun (_, (a1, b1)) (_, (a2, b2)) -> 
      if a1>a2 then 1 else if a1=a2 then 0 else (-1))

(** [print_red_or_black clst] is a string with sorted chess of the given [clst]. 
    It is sorted as the board, the first row is row1, downto row 10. 
    Within each row, the chesses are sorted according to their column. *) 
let print_red_or_black colorlst = 
  let rec helper row acc = 
    if row = 10 then acc^" \n \n "^(row_chess_sort row colorlst |> print_state)
    else let str = (row_chess_sort row colorlst |> print_state) in 
      if str = "" then helper (row+1) acc
      else helper (row+1) (acc^" \n \n "^str)
  in 
  helper 1 ""

(** [pos_helper1 lst] is the list of first elements in [lst] of tuples. *)
let rec pos_helper1 = function
  | [] -> []
  | h :: t -> fst h :: pos_helper1 t 

(** [pos_helper2 lst] is the list of second elements in [lst] of tuples. *)
let rec pos_helper2 = function
  | [] -> []
  | h :: t -> snd h :: pos_helper2 t

(** [player_black_round] is the round that black player move a chess. The player
    will need to input something like [move jiang-1 to (5,2)] in order to move 
    a valid chess. If it is an invalid input - Malformed, the player can input 
    again, i.e. the game will not terminate. Error message will tells that input 
    textual form is not correct (the name of the chess need to be correct, he 
    need to move his own chess not other's chess, it should contains key word 
    move <chessname-number> to <(valid position, valid position)>. 
    If it is an invalid position of the chess - Illegal, error message will ask 
    the player to input again and claim that this is not a valid position.*)
let rec player_black_round st = 
  let move_black_helper chessName nextLocation = 
    let b = Board.init_board in
    match (State.move nextLocation chessName Board.Black b st) with
    | exception (State.InvalidChess s) -> 
      begin 
        print_string "> "; 
        print_endline s;
        player_black_round st
      end
    | Illegal -> (print_string "> "; 
                  print_endline "You are not allowed to move to this location, please input another movement.";
                  player_black_round st)
    |Legal newst -> Draw.print_board true newst; print_endline ""; 
      begin 
        if List.mem ("Shuai",1) (State.current_red_captured newst)
        then (print_endline "The game has ended, Black wins.";) else
          try
            let shuaiPos = List.assoc ("Shuai", 1) 
                (State.red_remain_chess newst) in
            let jiangPos = List.assoc ("Jiang", 1)
                (State.black_remain_chess newst) in
            let chess_noSJ = List.remove_assoc ("Shuai", 1) 
                (State.black_remain_chess newst @ 
                 State.red_remain_chess newst) |> 
                             List.remove_assoc ("Jiang", 1) in 
            let remainPos = 
              pos_helper2 chess_noSJ in
            let colPos = pos_helper1 remainPos in
            if fst shuaiPos = fst jiangPos && 
               not (List.mem (fst shuaiPos) colPos) then
              (print_endline "The game has ended, Red wins.";) else 
              player_red_round newst
          with Not_found -> print_string"> ";
            print_endline "This should be impossible, the game should have already ended.";
      end 
  in 
  let rec yes_no_black_helper chessName nextLocation = 
    let inp = read_line() in 
    match inp with
    | "y" -> move_black_helper chessName nextLocation
    | "N" -> player_black_round st 
    | _ -> print_string "> Please enter y [yes], N [No]. "; 
      yes_no_black_helper chessName nextLocation
  in
  let rec yes_no_black_quit_helper () = 
    let inp = read_line() in 
    match inp with
    | "y" -> exit 0
    | "N" -> player_black_round st 
    | _ -> print_string "> Please enter y [yes], N [No]. "; 
      yes_no_black_quit_helper ()
  in
  print_string "> It is "; 
  ANSITerminal.(print_string [Bold] "Black");
  print_endline " turn";
  print_string "> ";
  let input_str = read_line() in 
  try let cmd = Command.parse input_str in 
    match cmd with
    | Move (chessName, nextLocation) -> move_black_helper chessName nextLocation
    | Quit -> exit 0
  with 
  | Command.Malformed s -> 
    begin 
      print_string "> "; 
      print_endline (s^"Please input your next movement in the correct form.");
      player_black_round st
    end 
  | Command.DoYouMean (s, (chessName, nextLocation)) -> 
    begin 
      print_string "> ";
      print_endline s;
      print_string "> ";
      yes_no_black_helper chessName nextLocation
    end 
  | Command.DoYouQuit -> 
    begin 
      print_string "> ";
      print_endline "Did you mean quit? [y/N]";
      print_string "> ";
      yes_no_black_quit_helper ()
    end
  | Command.Empty -> 
    begin 
      print_string "> "; 
      print_endline "Your input is empty, pleae enter a valid movement [ move <chess_name> - <chess_id> to (<column position>, <row position>) ].";
      player_black_round st
    end 

(** [player_red_round] is the round that red player move a chess. The player
    will need to input something like [move shuai-1 to (5,9)] in order to move 
    a valid chess. If it is an invalid input - Malformed, the player can input 
    again, i.e. the game will not terminate. Error message will tells that input 
    textual form is not correct (the name of the chess need to be correct, he 
    need to move his own chess not other's chess, it should contains key word 
    move <chessname-number> to <(valid position, valid position)>. 
    If it is an invalid position of the chess - Illegal, error message will ask 
    the player to input again and claim that this is not a valid position.*)
and player_red_round st = 
  let move_red_helper chessName nextLocation = 
    let b = Board.init_board in
    match (State.move nextLocation chessName Board.Red b st) with
    | exception (State.InvalidChess s) -> 
      begin 
        print_string "> "; 
        print_endline s;
        player_red_round st
      end 
    | Illegal -> (print_string "> "; 
                  print_endline "You are not allowed to move to this location, please input another movement.";
                  player_red_round st)
    |Legal newst -> Draw.print_board true newst; print_endline "";
      begin 
        if List.mem ("Jiang",1) (State.current_black_captured newst) 
        then (print_endline "The game has ended, Red wins.";) else
          try
            let shuaiPos = List.assoc ("Shuai", 1) 
                (State.red_remain_chess newst) in
            let jiangPos = List.assoc ("Jiang", 1)
                (State.black_remain_chess newst) in
            let chess_noSJ = List.remove_assoc ("Shuai", 1) 
                (State.black_remain_chess newst @ 
                 State.red_remain_chess newst) |> 
                             List.remove_assoc ("Jiang", 1) in 
            let remainPos = 
              pos_helper2 chess_noSJ in
            let colPos = pos_helper1 remainPos in
            if fst shuaiPos = fst jiangPos && 
               not (List.mem (fst shuaiPos) colPos) then
              (print_endline "The game has ended, Black wins.";) else 
              player_black_round newst
          with Not_found -> print_string"> ";
            print_endline "This should be impossible, the game should have already ended.";
      end
  in 
  let rec yes_no_red_helper chessName nextLocation = 
    let inp = read_line() in 
    match inp with
    | "y" -> move_red_helper chessName nextLocation
    | "N" -> player_red_round st 
    | _ -> print_string "> Please enter y [yes], N [No]. "; 
      yes_no_red_helper chessName nextLocation
  in
  let rec yes_no_red_quit_helper () = 
    let inp = read_line() in 
    match inp with
    | "y" -> exit 0
    | "N" -> player_red_round st 
    | _ -> print_string "> Please enter y [yes], N [No]. "; 
      yes_no_red_quit_helper ()
  in
  print_string "> It is ";
  ANSITerminal.(print_string [red; Bold] "Red");
  print_endline " turn";
  print_string "> ";
  let input_str = read_line() in 
  try let cmd = Command.parse input_str in 
    match cmd with
    | Move (chessName, nextLocation) -> move_red_helper chessName nextLocation
    | Quit -> exit 0
  with 
  | Command.Malformed s -> (print_string "> "; 
                            print_endline (s^"Please input your next movement in the correct format. ");
                            player_red_round st)
  | Command.DoYouMean (s, (chessName, nextLocation)) -> 
    begin 
      print_string "> ";
      print_endline s;
      print_string "> ";
      yes_no_red_helper chessName nextLocation
    end 
  | Command.DoYouQuit -> 
    begin 
      print_string "> ";
      print_endline "Did you mean quit? [y/N]";
      print_string "> ";
      yes_no_red_quit_helper ()
    end
  | Command.Empty -> 
    begin 
      print_string "> "; 
      print_endline "Your input is empty, pleae enter a valid movement [ move <chess_name> - <chess_id> to (<column position>, <row position>) ].";
      player_red_round st 
    end 

(** [play_game f] starts the game with initial state, i.e. the original 
    chesses' position on a board. *)
let play_game start =
  print_endline "The game has started.";
  Draw.print_board true State.init_state;
  print_endline "";
  player_red_round State.init_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [cyan]
                  ("\n\n Welcome to the Chinese Chess Game! " ^ 
                   "\n" ^ " \xe4\xba\xba\xe7\x94\x9f\xe5\xa6\x82\xe6\xa3\x8b\xef\xbc\x8c\xe8\x90\xbd\xe5\xad\x90\xe7\x84\xa1\xe6\x82\x94" ^
                   "\n Translation: No Regret Is Allowed. " ^
                   "\n The red player should move first. \n"));
  print_endline "press any key to play";
  print_string  "> ";
  match read_line() with
  | start -> play_game start 

(* Execute the game engine. *)
let () = main ()
