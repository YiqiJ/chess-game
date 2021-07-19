(** [che1] is the Chinese character for che1. *)
let che1 = "\xe8\xbb\x8a"^"1"

(** [che2] is the Chinese character for che2. *)
let che2 = "\xe8\xbb\x8a"^"2" 

(** [ma1] is the Chinese character for ma1. *)
let ma1 = "\xe9\xa6\xac"^"1" 

(** [ma2] is the Chinese character for ma2. *)
let ma2 = "\xe9\xa6\xac"^"2"   

(** [xiang1] is the Chinese character for xiang1. *)
let xiang1 = "\xe7\x9b\xb8"^"1"  

(** [xiang2] is the Chinese character for xiang2. *)
let xiang2 = "\xe7\x9b\xb8"^"2" 

(** [shi1] is the Chinese character for shi1. *)
let shi1 = "\xe4\xbb\x95"^"1"   

(** [shi2] is the Chinese character for shi2. *)
let shi2 = "\xe4\xbb\x95"^"2"   

(** [shuai] is the Chinese character for shuai. *)
let shuai = "\xe5\xb8\xa5"^"1" 

(** [pao1] is the Chinese character for pao1. *)
let pao1 = "\xe7\x82\xae"^"1"   

(** [pao2] is the Chinese character for pao2. *)
let pao2 = "\xe7\x82\xae"^"2"  

(** [bing1] is the Chinese character for bing1. *)
let bing1 = "\xe5\x85\xb5"^"1"  

(** [bing2] is the Chinese character for bing2. *)
let bing2 = "\xe5\x85\xb5"^"2"  

(** [bing3] is the Chinese character for bing3. *)
let bing3 = "\xe5\x85\xb5"^"3"  

(** [bing4] is the Chinese character for bing4. *)
let bing4 = "\xe5\x85\xb5"^"4"

(** [bing5] is the Chinese character for bing5. *)
let bing5 = "\xe5\x85\xb5"^"5"

(** [che3] is the Chinese character for che3. *)
let che3 = "\xe8\xbb\x8a"^"3"   

(** [che4] is the Chinese character for che4. *)
let che4 = "\xe8\xbb\x8a"^"4" 

(** [ma3] is the Chinese character for ma3. *)
let ma3 = "\xe9\xa6\xac"^"3"  

(** [ma4] is the Chinese character for ma4. *)
let ma4 = "\xe9\xa6\xac"^"4"   

(** [xiang3] is the Chinese character for xiang3. *)
let xiang3 = "\xe8\xb1\xa1"^"3" 

(** [xiang4] is the Chinese character for xiang4. *)
let xiang4 = "\xe8\xb1\xa1"^"4" 

(** [shi3] is the Chinese character for shi3. *)
let shi3 = "\xe5\xa3\xab"^"3"   

(** [shi4] is the Chinese character for shi4. *)
let shi4 = "\xe5\xa3\xab"^"4"   

(** [jiang] is the Chinese character for jiang. *)
let jiang = "\xe5\xb0\x87"^"1"  

(** [pao3] is the Chinese character for pao3. *)
let pao3 = "\xe7\x82\xae"^"3"  

(** [pao4] is the Chinese character for pao4. *)
let pao4 = "\xe7\x82\xae"^"4"  

(** [zu1] is the Chinese character for zu1. *)
let zu1 = "\xe5\x8d\x92"^"1"    

(** [zu2] is the Chinese character for zu2. *)
let zu2 = "\xe5\x8d\x92"^"2"  

(** [zu3] is the Chinese character for zu3. *)
let zu3 = "\xe5\x8d\x92"^"3"    

(** [zu4] is the Chinese character for zu4. *)
let zu4 = "\xe5\x8d\x92"^"4"    

(** [zu5] is the Chinese character for zu5. *)
let zu5 = "\xe5\x8d\x92"^"5"    

(** [row_chess_red rownum st] is the red chess in utf-8 and col that should be 
    placed on [rownum], where [rownum] must satisfy mod 3 equal to 1 and 
    row = (rownum - 1) / 3 + 1. *)
let row_chess_red rownum st = 
  let red = State.red_remain_chess st in 
  let board_row = (rownum-1)/3 + 1 in 
  let rec helper red acc = 
    match red with 
    | [] -> acc 
    | ((name, id), (col, row))::t -> 
      if row=board_row then 
        begin 
          if name="Che" && id=1 then helper t ((che1, col)::acc)
          else if name="Che" && id=2 then helper t ((che2, col)::acc)
          else if name="Ma" && id=1 then helper t ((ma1, col)::acc)
          else if name="Ma" && id=2 then helper t ((ma2, col)::acc)
          else if name="Xiang" && id=1 then helper t ((xiang1, col)::acc)
          else if name="Xiang" && id=2 then helper t ((xiang2, col)::acc)
          else if name="Shi" && id=1 then helper t ((shi1, col)::acc)
          else if name="Shi" && id=2 then helper t ((shi2, col)::acc)
          else if name="Shuai" && id=1 then helper t ((shuai, col)::acc)
          else if name="Pao" && id=1 then helper t ((pao1, col)::acc)
          else if name="Pao" && id=2 then helper t ((pao2, col)::acc)
          else if name="Bing" && id=1 then helper t ((bing1, col)::acc)
          else if name="Bing" && id=2 then helper t ((bing2, col)::acc)
          else if name="Bing" && id=3 then helper t ((bing3, col)::acc)
          else if name="Bing" && id=4 then helper t ((bing4, col)::acc)
          else if name="Bing" && id=5 then helper t ((bing5, col)::acc)
          else failwith "invalid current red chesses format in row_chess_red draw"
        end 
      else helper t acc
  in 
  helper red []

(** [row_chess_blacks rownum st] is the black chess in utf-8 and col that should 
    be placed on [rownum], where [rownum] must satisfy mod 3 equal to 1 and 
    row = (rownum - 1) / 3 + 1. *)
let row_chess_black rownum st = 
  let black = State.black_remain_chess st in 
  let board_row = (rownum-1)/3 + 1 in 
  let rec helper red acc = 
    match red with 
    | [] -> acc 
    | ((name, id), (col, row))::t -> 
      if row=board_row then 
        begin 
          if name="Che" && id=3 then helper t ((che3, col)::acc)
          else if name="Che" && id=4 then helper t ((che4, col)::acc)
          else if name="Ma" && id=3 then helper t ((ma3, col)::acc)
          else if name="Ma" && id=4 then helper t ((ma4, col)::acc)
          else if name="Xiang" && id=3 then helper t ((xiang3, col)::acc)
          else if name="Xiang" && id=4 then helper t ((xiang4, col)::acc)
          else if name="Shi" && id=3 then helper t ((shi3, col)::acc)
          else if name="Shi" && id=4 then helper t ((shi4, col)::acc)
          else if name="Jiang" && id=1 then helper t ((jiang, col)::acc)
          else if name="Pao" && id=3 then helper t ((pao3, col)::acc)
          else if name="Pao" && id=4 then helper t ((pao4, col)::acc)
          else if name="Zu" && id=1 then helper t ((zu1, col)::acc)
          else if name="Zu" && id=2 then helper t ((zu2, col)::acc)
          else if name="Zu" && id=3 then helper t ((zu3, col)::acc)
          else if name="Zu" && id=4 then helper t ((zu4, col)::acc)
          else if name="Zu" && id=5 then helper t ((zu5, col)::acc)
          else failwith "invalid current black chesses format in row_chess_black draw"
        end 
      else helper t acc 
  in 
  helper black []

(** [row_chess rownum st] are the chesses in a row sorted by col index. *)
let row_chess rownum st = 
  let r = row_chess_red rownum st in 
  let b = row_chess_black rownum st in 
  let red = List.map (fun elt -> (Board.Red, elt)) r in 
  let black = List.map (fun elt -> (Board.Black, elt)) b in 
  red @ black |> List.stable_sort (fun (_, (_, col1)) (_, (_, col2)) 
                                    -> if col1>col2 then 1 else (-1))

(** [row_print rownum st] prints the row that can have chesses on it, i.e., row 
    with (rownum-1) mod 3 = 0. *)
let row_print rownum st = 
  let chess_col = row_chess rownum st in 
  let row = (rownum-1)/3 + 1 in 
  let () = if row<>10 
    then ANSITerminal.(print_string [white] ("\n" ^ " " ^ (string_of_int row) 
                                             ^ "   "))
    else ANSITerminal.(print_string [white] ("\n" ^ (string_of_int row) 
                                             ^ "   ")) in 
  let rec helper chess_col n = 
    match chess_col with 
    | [] -> 
      begin 
        if n=1 then let () = ANSITerminal.(print_string [white] "  +----") 
          in helper chess_col (n+1)
        else if n=9 then ANSITerminal.(print_string [white] "-+")
        else let () = ANSITerminal.(print_string [white] "-+----") 
          in helper chess_col (n+1)
      end 
    | (Board.Red, (chess, col))::t -> 
      begin 
        if n=col && col=1 
        then let () = ANSITerminal.(print_string [red] (" "^chess)); 
               ANSITerminal.(print_string [white] "---") in helper t (n+1)
        else if n=col && col=9 then ANSITerminal.(print_string [red] chess)
        else if n=col && col=n 
        then let () = ANSITerminal.(print_string [red] chess); 
               ANSITerminal.(print_string [white] "---") in helper t (n+1)
        else if n=1 
        then let () = ANSITerminal.(print_string [white] "  +----") 
          in helper chess_col (n+1)
        else if n=9 then ANSITerminal.(print_string [white] "-+")
        else let () = ANSITerminal.(print_string [white] "-+----") 
          in helper chess_col (n+1)
      end 
    | (Board.Black, (chess, col))::t -> 
      begin 
        if n=col && col=1 
        then let () = ANSITerminal.(print_string [black] (" "^chess)); 
               ANSITerminal.(print_string [white] "---") in helper t (n+1)
        else if n=col && col=9 then ANSITerminal.(print_string [black] chess)
        else if n=col && col=n 
        then let () = ANSITerminal.(print_string [black] chess); 
               ANSITerminal.(print_string [white] "---") in helper t (n+1)
        else if n=1 
        then let () = ANSITerminal.(print_string [white] "  +----") 
          in helper chess_col (n+1)
        else if n=9 then ANSITerminal.(print_string [white] "-+")
        else let () = ANSITerminal.(print_string [white] "-+----") 
          in helper chess_col (n+1)
      end 
  in 
  helper chess_col 1

let print_board b st = 
  if b then 
    let rec helper rownum = 
      if rownum = (-1) 
      then let () = ANSITerminal.(print_string [white] "\n       1     2     3     4     5     6     7     8     9") 
        in helper (rownum+1)
      else if rownum = 0 
      then let () = ANSITerminal.(print_string [white] "\n") 
        in helper (rownum+1)

      else if rownum = 2 || rownum = 23 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     | \\   |   / |     |     |     |") 
        in helper (rownum+1) 
      else if rownum = 3 || rownum = 24 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |   \\ | /   |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 5 || rownum = 26 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |   / | \\   |     |     |     |") 
        in helper (rownum+1)
      else if rownum = 6 || rownum = 27 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     | /   |   \\ |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 8 || rownum = 9 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |     |     |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 11 || rownum = 12 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |     |     |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 14 
      then let () = ANSITerminal.(printf [white] "\n       |          %s    %s              ~ ~ ~          |") "\xe6\xa5\x9a" "\xe6\xb2\xb3" 
        in helper (rownum+1)
      else if rownum = 15 
      then let () = ANSITerminal.(printf [white] "\n       |           ~ ~ ~                %s    %s       |") "\xe6\xb1\x89" "\xe7\x95\x8c" 
        in helper (rownum+1) 

      else if rownum = 17 || rownum = 18 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |     |     |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 20 || rownum = 21 
      then let () = ANSITerminal.(printf [white] "\n       |     |     |     |     |     |     |     |     |") 
        in helper (rownum+1)

      else if rownum = 28 then row_print rownum st

      else let () = row_print rownum st in helper (rownum+1)
    in 
    helper (-1)
  else ()
