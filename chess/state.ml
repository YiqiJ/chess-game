(* Note: You may introduce new code anywhere in this file. *) 

(* TODO: replace [unit] with a type of your own design. *)
type t = {red_remain_chess : (Board.name*(int*int)) list; 
          black_remain_chess : (Board.name*(int*int)) list}

let init_red chesslst = 
  let rec helper chesslst acc = 
    match chesslst with 
    | [] -> acc 
    | h::t -> if (Board.color_chess h)=Board.Red 
      then helper t (((Board.name_chess h), (Board.pos_chess h))::acc)
      else helper t acc
  in 
  helper chesslst []

let init_black chesslst = 
  let rec helper chesslst acc = 
    match chesslst with 
    | [] -> acc 
    | h::t -> if (Board.color_chess h)=Board.Black 
      then helper t (((Board.name_chess h), (Board.pos_chess h))::acc)
      else helper t acc
  in 
  helper chesslst []

let init_state =
  let chesses = Board.chesses (Board.init_board) in 
  let red = init_red chesses in 
  let black = init_black chesses in 
  {red_remain_chess = red; black_remain_chess=black}

let red_remain_chess st =
  st.red_remain_chess

let black_remain_chess st =
  st.black_remain_chess

type result = Legal of t | Illegal

exception InvalidChess of string

let pos_lst remain = 
  let rec helper remain acc = 
    match remain with 
    | [] -> acc
    | h::t -> helper t (snd h::acc)
  in 
  helper remain []

(** [red_remian_chess_pos st] is a list of positions of remaining red chess 
    on board in state [st]. *)
let red_remain_pos st= 
  let rec red_remian_chess_pos_help lst accu = 
    match lst with
    |[] -> accu
    |(h1, h2)::t -> red_remian_chess_pos_help t (h2::accu)
  in
  red_remian_chess_pos_help (red_remain_chess st) []

(** [black_remain_chess_pos st ]a list of positions of remaining black 
    chess on board in state [st]. *)
let black_remain_pos st = 
  let rec black_remain_chess_help lst accu =
    match lst with
    |[] -> accu
    |(h1, h2) ::t -> black_remain_chess_help t (h2::accu)
  in
  black_remain_chess_help (black_remain_chess st) []

(** [occupied_by_red pos st] is true if the given position [pos] is occupied by 
    a red chess in state [st]. *)
let occupied_by_red pos st=
  let rec occupied_by_red_help pos lst= 
    match lst with
    |[] -> false
    |(a,b)::t -> if a = (fst pos) && b =(snd pos) then true 
      else occupied_by_red_help pos t
  in
  occupied_by_red_help pos (red_remain_pos st)

(** [occupied_by_black pos st] is true if the given position [pos] is occupied 
    by a black chess in state [st]*)
let occupied_by_black pos st=
  let rec occupied_by_black_help pos lst =
    match lst with 
    |[] -> false
    |(a,b)::t -> if a = (fst pos) && b = (snd pos) then true 
      else occupied_by_black_help pos t
  in
  occupied_by_black_help pos (black_remain_pos st)

(** [red_pao_fly lst accu st] is the list of positions red pao can fly to. *)
let rec red_pao_fly lst accu st=
  match lst with
  |[] -> accu
  |h::t -> if (occupied_by_black h st) then (h::accu) 
    else if (occupied_by_red h st) then accu
    else red_pao_fly t accu st

(** [red_pao_move_furthuer lst accu st] is the list of positions red pao can 
    move further to. *)
let rec red_pao_move_further lst accu st =
  match lst with
  |[] -> accu
  |h::t -> if (occupied_by_black h st) || (occupied_by_red h st)
    then (red_pao_fly t [] st) @ accu
    else red_pao_move_further t (h::accu) st

(** [red_pao_move_help lst st] is the helper function for moving red pao. *)
let red_pao_move_help lst st=
  match lst with
  |[] -> []
  |h::t -> red_pao_move_further lst [] st

(** [red_pao_move_less_b] is the list of position that red pao can move to with
    the second pos index decreased. *)
let red_pao_move_less_b st pos = 
  let a = fst pos in 
  let b = snd pos in
  red_pao_move_help (List.rev(List.filter (fun h -> (snd h) < b) 
                                [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);
                                 (a,8);(a,9);(a,10)])) st

(** [red_pao_move_more_b] is the list of position that red pao can move to with
    the second pos index increased. *)
let red_pao_move_more_b st pos =
  let a = fst pos in
  let b = snd pos in
  red_pao_move_help (List.filter (fun h -> (snd h) > b) 
                       [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);(a,9);
                        (a,10)]) st

(** [red_pao_move_less_a] is the list of position that red pao can move to with
    the first pos index decreased. *)
let red_pao_move_less_a st pos = 
  let a = fst pos in
  let b = snd pos in
  red_pao_move_help (List.rev (List.filter (fun h -> (fst h) < a) 
                                 [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);
                                  (8,b);(9,b)])) st

(** [red_pao_move_more_a] is the list of position that red pao can move to with
    the first pos index increased. *)
let red_pao_move_more_a st pos =
  let a = fst pos in
  let b = snd pos in
  red_pao_move_help (List.filter (fun h -> (fst h) > a) 
                       [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);
                        (9,b)]) st

(** [black_pao_fly lst accu st] is the list of positions black pao can fly to.*)
let rec black_pao_fly lst accu st=
  match lst with
  |[] -> accu
  |h::t -> if (occupied_by_red h st) then (h::accu) 
    else if (occupied_by_black h st) then accu
    else black_pao_fly t accu st

(** [black_pao_move_furthuer lst accu st] is the list of positions black pao can 
    move further to. *)
let rec black_pao_move_further lst accu st=
  match lst with
  |[] -> accu
  | h::t -> if (occupied_by_black h st) || (occupied_by_red h st)
    then (black_pao_fly t [] st) @ accu
    else black_pao_move_further t (h::accu) st

(** [black_pao_move_help lst st] is the helper function for moving black pao. *)
let black_pao_move_help lst st=
  match lst with
  |[] -> []
  |h::t -> black_pao_move_further lst [] st

(** [black_pao_move_less_b] is the list of position that black pao can move to 
    with the second pos index decreased. *)
let black_pao_move_less_b st pos = 
  let a = fst pos in 
  let b = snd pos in
  black_pao_move_help (List.rev(List.filter (fun h -> (snd h) < b) 
                                  [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);
                                   (a,8);(a,9);(a,10)])) st

(** [black_pao_move_more_b] is the list of position that black pao can move to 
    with the second pos index increased. *)
let black_pao_move_more_b st pos = 
  let a = fst pos in
  let b = snd pos in
  black_pao_move_help (List.filter (fun h -> (snd h) > b) 
                         [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);(a,9);
                          (a,10)]) st

(** [black_pao_move_less_a] is the list of position that black pao can move to
    with the first pos index decreased. *)
let black_pao_move_less_a st pos =
  let a = fst pos in
  let b = snd pos in
  black_pao_move_help (List.rev (List.filter (fun h -> (fst h) < a) 
                                   [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);
                                    (8,b);(9,b)])) st

(** [black_pao_move_more_a] is the list of position that black pao can move to
    with the first pos index increased. *)
let black_pao_move_more_a st pos =
  let a = fst pos in
  let b = snd pos in
  black_pao_move_help (List.filter (fun h -> (fst h) > a) 
                         [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);
                          (9,b)]) st 

(** [black_move_further lst accu st] is the list of positions 
    that the black Che can move to among positions in [lst] in state [st],
    given that [lst] is not empty. *)
let rec black_move_further lst accu st=
  match lst with
  |[] -> accu
  |h::t -> if (occupied_by_black h st) = false && (occupied_by_red h st) = false  
    then black_move_further t (h::accu) st
    else if (occupied_by_red h st) = true 
    then h::accu
    else accu

(** [black_move_help lst st] is the list of positions 
    that the black Che can move to among positions in [lst] in state [st]. *)
let black_move_help lst st= 
  match lst with
  |[] -> []
  |h::t -> black_move_further lst [] st

(**[black_move_less_b st pos] is list of valid position that Che can be moved to 
    in the negative b direction from current position [pos] in state [st]. *)
let black_move_less_b st pos= 
  let a = fst pos in
  let b = snd pos in
  black_move_help (List.rev(List.filter (fun h -> (snd h) < b) 
                              [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);
                               (a,9);(a,10)])) st

(**[black_move_more_b st pos] is list of valid position that Che can be moved to 
    in the positive b direction from current position [pos] in state [st]. *)
let black_move_more_b st pos =
  let a = fst pos in 
  let b = snd pos in 
  black_move_help (List.filter (fun h -> (snd h) > b) 
                     [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);(a,9);
                      (a,10)]) st

(**[black_move_less_a st pos] is list of valid position that Che can be moved to 
    in the negative a direction from current position [pos] in state [st]. *)
let black_move_less_a st pos = 
  let a = fst pos in
  let b = snd pos in 
  black_move_help (List.rev (List.filter (fun h -> (fst h) < a)
                               [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);
                                (9,b)])) st

(**[black_move_more_a st pos] is list of valid position that Che can be moved to 
    in the positive a direction from current position [pos] in state [st]. *)
let black_move_more_a st pos =
  let a = fst pos in
  let b = snd pos in
  black_move_help (List.filter (fun h -> (fst h) > a)
                     [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);(9,b)]) st

(** [red_move_further lst accu st] is the list of positions that the red Che can 
    move to among positions in [lst] in state [st], given that [lst] is not 
    empty. *)
let rec red_move_further lst accu st = 
  match lst with
  |[] -> accu
  |h::t -> if (occupied_by_red h st) = false && (occupied_by_black h st) = false 
    then red_move_further t (h::accu) st
    else if (occupied_by_black h st) = true
    then h::accu
    else accu

(** [red_move_help lst st] is the list of positions that the red Che can move to 
    among positions in [lst] in state [st]. *)
let red_move_help lst st =
  match lst with
  |[] -> []
  |h::t -> red_move_further lst [] st

(**[red_move_less_b st pos] is list of valid position that Che can be moved to 
    in the negative b direction from current position [pos] in state [st]. *)
let red_move_less_b st pos =
  let a = fst pos in
  let b = snd pos in
  red_move_help (List.rev (List.filter (fun h -> (snd h) < b)
                             [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);
                              (a,9);(a,10)])) st

(**[red_move_more_b st pos] is list of valid position that Che can be moved to 
    in the positive b direction from current position [pos] in state [st]. *)
let red_move_more_b st pos = 
  let a = fst pos in 
  let b = snd pos in 
  red_move_help (List.filter (fun h -> (snd h) > b)
                   [(a,1);(a,2);(a,3);(a,4);(a,5);(a,6);(a,7);(a,8);(a,9);
                    (a,10)]) st

(**[red_move_less_a st pos] is list of valid position that Che can be moved to 
    in the negative a direction from current position [pos] in state [st]. *)
let red_move_less_a st pos =
  let a = fst pos in 
  let b = snd pos in 
  red_move_help (List.rev (List.filter (fun h -> (fst h) < a)
                             [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);
                              (9,b)])) st

(**[red_move_more_a st pos] is list of valid position that Che can be moved to 
    in the positive a direction from current position [pos] in state [st]. *)
let red_move_more_a st pos = 
  let a = fst pos in 
  let b = snd pos in 
  red_move_help (List.filter (fun h -> (fst h) > a)
                   [(1,b);(2,b);(3,b);(4,b);(5,b);(6,b);(7,b);(8,b);(9,b)]) st


(** [next_valid_bing current_pos st] is a list of next valid position of chess 
    Bing. If there are Red color chess in the potential valid position, that 
    position becomes invalid. However, if there are Black color chess in the 
    potential valid position, that position is still valid. Bing can only move 
    forward when it has not crossed the river. It can move forward, left, or 
    right after it cross the river. It can never move backward.
    Require: current_pos is a valid position.  *)
let next_valid_bing current_pos st= 
  let a = fst current_pos in 
  let b = snd current_pos in
  let red_pos = st.red_remain_chess |> pos_lst in 
  let potential = 
    if b >= 6 && b <= 7 then [(a, b-1)]
    else if b<= 5 && b > 1 then begin 
      if a >= 2 && a <= 8 then [(a-1, b); (a+1,b); (a, b-1)]
      else if a = 1 then [(a+1, b); (a, b-1)]
      else if a = 9 then [(a-1, b); (a, b-1)] 
      else failwith "impossible" end 
    else if b = 1 then begin 
      if a >= 2 && a <= 8 then [(a-1, b); (a+1,b)]
      else if a = 1 then [(a+1, b)]
      else if a = 9 then [(a-1, b)]
      else failwith "impossible" end
    else failwith "impossible"
  in List.filter (fun elt -> not (List.mem elt red_pos)) potential

(** [next_valid_zu current_pos st] is a list of next valid position of chess 
    Zu. If there are Black color chess in the potential valid position, that 
    position becomes invalid. However, if there are Red color chess in the 
    potential valid position, that position is still valid. Zu can only move 
    forward when it has not crossed the river. It can move forward, left, or 
    right after it cross the river. It can never move backward.
    Require: current_pos is a valid position.  *)
let next_valid_zu current_pos st= 
  let a = fst current_pos in
  let b = snd current_pos in
  let black = st.black_remain_chess in 
  let black_pos = pos_lst black in 
  let potential =
    if b<= 5 && b >= 4 then [(a, b+1)]
    else if b>= 6 && b < 10 then begin 
      if a >= 2 && a <= 8 then [(a-1, b);(a+1,b);(a,b+1)]
      else if a = 1 then [(a+1, b);(a, b+1)]
      else if a =9 then [(a-1, b);(a, b+1)]
      else failwith "impossible" end 
    else if b = 10 then begin 
      if a >= 2 && a <= 8 then [(a-1, b);(a+1,b)]
      else if a =1 then [(a+1, b)]
      else if a = 9 then [(a-1, b)]
      else failwith "impossible" end 
    else failwith "impossible"
  in List.filter (fun elt -> not (List.mem elt black_pos)) potential

(** [redxiang_pos a b x y f st lst current_pos accu]
    is whether the lower right position should be add to valid next positions list
    based on current x [a]; current y [b]; next x [x]; next y [y]; function for 
    recursion [f]; current state [st]; current position [current_pos]; 
    list of already valified next positions [accu]. *)
let redxiang_pos a b x y f st lst current_pos accu=
  if occupied_by_red (a, b) st || occupied_by_black (a, b) st
  then f current_pos lst accu st
  else if occupied_by_red (x, y) st 
  then f current_pos lst accu st
  else f current_pos lst ((x,y)::accu) st

(** [next_valid_xiang_red current_pos st] is a list of next valid position of 
    chess Xiang. If there are Red color chess in the potential valid position, 
    that position becomes invalid. However, if there are Black color chess in 
    the potential valid position, that position is still valid. Let a denotes 
    the x value of Xiang's current position, and let b denotes the y value of 
    Xiang's current position. Xiang can only move to four positions: (a-2,b+2)
    if it is on the board and (a-1,b+1) is not occupied by either a Red chess 
    or a black chess;(a-2,b-2) if it is on the board and (a-1,b-1) is not 
    occupied by either a Red chess or a black chess;(a+2,b+2)if it is on the 
    board and (a+1,b+1) is not occupied by either a Red chess or a black chess;
    (a+2,b-2)if it is on the board and (a+1,b-1) is not occupied by either 
    a Red chess or a black chess; Xiang cannot across the rive. 
    Require: current_pos is a valid position.  *)
let rec next_xiang_red_help current_pos lst accu st=
  match lst with
  | [] -> accu
  | (x,y)::t -> 
    let a = fst current_pos in let b = snd current_pos in
    if x >= 1 && x <= 9 && y >= 1 && y <= 10 then
      begin 
        if x-a = -2 && y-b = 2 then
          redxiang_pos (a-1) (b+1) x y (next_xiang_red_help) 
            st t current_pos accu  
        else if x-a = -2 && y-b = -2 then
          redxiang_pos (a-1) (b-1) x y (next_xiang_red_help) 
            st t current_pos accu
        else if x-a = 2 && y-b = 2 then
          redxiang_pos (a+1) (b+1) x y (next_xiang_red_help) 
            st t current_pos accu
        else 
          redxiang_pos (a+1) (b-1) x y (next_xiang_red_help) 
            st t current_pos accu
      end 
    else next_xiang_red_help current_pos t accu st

(** [next_valid_xiang_red current_pos st] returns a list of postions Xiang-1 or 
    Xiang-2 can move to based on its current position [current_pos] in current 
    state [st]. *)
let next_valid_xiang_red current_pos st= 
  let a = fst current_pos in
  let b = snd current_pos in
  next_xiang_red_help current_pos 
    [(a-2, b+2);(a-2, b-2);(a+2, b+2);(a+2, b-2)]
    [] st

(**[blackxiang_pos a b x y f st lst current_pos accu]
   is whether the upper right position should be add to valid next positions 
   list based on current x [a]; current y [b]; next x [x]; next y [y]; function 
   for recursion [f]; current state [st]; current position [current_pos]; 
   list of already valified next positions [accu]. *)
let blackxiang_pos a b x y f st lst current_pos accu=
  if occupied_by_red (a,b) st ||occupied_by_black (a,b) st
  then f current_pos lst accu st
  else if occupied_by_black (x, y) st 
  then f current_pos lst accu st
  else f current_pos lst ((x,y)::accu) st

(** [next_xiang_black_help current_pos lst accu st] is the list of valid 
    next positions [accu] determined based on Black Xiang's current position 
    [current_pos] and the list of possibly valid locations [lst] in current 
    state [st]*)
let rec next_xiang_black_help current_pos lst accu st =
  match lst with
  |[] -> accu
  |(x,y)::t -> 
    let a = fst current_pos in let b = snd current_pos in
    if x >=1 && x <= 9 && y>= 1 && y <= 10 then
      begin 
        if x-a = -2 && y-b = 2 then 
          blackxiang_pos (a-1) (b+1) x y (next_xiang_black_help) 
            st t current_pos accu
        else if x-a = -2 && y-b = -2 then
          blackxiang_pos (a-1) (b-1) x y (next_xiang_black_help) 
            st t current_pos accu
        else if x-a = 2 && y-b = 2 then
          blackxiang_pos (a+1) (b+1) x y (next_xiang_black_help) 
            st t current_pos accu
        else
          blackxiang_pos (a+1) (b-1) x y (next_xiang_black_help) 
            st t current_pos accu
      end
    else next_xiang_black_help current_pos t accu st

(** [next_valid_xiang_black current_pos st] is a list of next valid position of 
    chess Xiang. If there are Black color chess in the potential valid position, 
    that position becomes invalid. However, if there are Red color chess in 
    the potential valid position, that position is still valid. Let a denotes 
    the x value of Xiang's current position, and let b denotes the y value of 
    Xiang's current position. Xiang can only move to four positions: (a-2,b+2)
    if it is on the board and (a-1,b+1) is not occupied by either a Red chess 
    or a black chess;(a-2,b-2) if it is on the board and (a-1,b-1) is not 
    occupied by either a Red chess or a black chess;(a+2,b+2)if it is on the 
    board and (a+1,b+1) is not occupied by either a Red chess or a black chess;
    (a+2,b-2)if it is on the board and (a+1,b-1) is not occupied by either 
    a Red chess or a black chess; Xiang cannot across the rive. 
    Require: current_pos is a valid position.  *)
let next_valid_xiang_black current_pos st = 
  let a = fst current_pos in
  let b = snd current_pos in
  next_xiang_black_help current_pos 
    [(a-2, b+2);(a-2, b-2);(a+2, b+2);(a+2, b-2)]
    [] st

(** [redma_pos a b x y f st lst current_pos accu] is the position of red ma. *)
let redma_pos a b x y f st lst current_pos accu=
  if occupied_by_red (a, b) st || occupied_by_black (a, b) st
  then f current_pos lst accu st
  else if occupied_by_red (x, y) st 
  then f current_pos lst accu st
  else f current_pos lst ((x,y)::accu) st

(**[ma_red_help current_pos lst accu st] is the list of positions 
   the red Ma can move to among positions in the list [lst] 
   from current position [current_pos] in state [st]. *)
let rec ma_red_help current_pos lst accu st=
  match lst with
  | [] -> accu
  | (x,y)::t -> 
    let a = fst current_pos in
    let b = snd current_pos in
    if x >= 1 && x <= 9 && y >= 1 && y <= 10 then
      begin 
        if x-a = -2 then
          redma_pos (a-1) b x y (ma_red_help) st t current_pos accu  
        else if x-a = 2 then
          redma_pos (a+1) b x y (ma_red_help) st t current_pos accu
        else if y-b = 2 then
          redma_pos a (b+1) x y (ma_red_help) st t current_pos accu
        else 
          redma_pos a (b-1) x y (ma_red_help) st t current_pos accu
      end 
    else ma_red_help current_pos t accu st

(** [ma_red current_pos st] is positions the red Ma can move to from
    current position [current_pos] in state [st]. *)
let next_valid_ma_red current_pos st = 
  let a = fst current_pos in
  let b = snd current_pos in
  ma_red_help current_pos 
    [(a-2, b+1);(a-2, b-1);(a+2, b+1);(a+2, b-1);(a-1,b+2);(a+1,b+2);(a-1,b-2);
     (a+1,b-2)]
    [] st

(**[ma_black_help current_pos lst accu st] is the list of positions 
   the black Ma can move to among positions in the list [lst] from current 
   position [current_pos] in state [st]. *)
let rec ma_black_help current_pos lst accu st =
  let helper t x y a b = 
    if occupied_by_red (a,b) st ||occupied_by_black (a,b) st
    then ma_black_help current_pos t accu st
    else if occupied_by_black (x, y) st 
    then ma_black_help current_pos t accu st
    else ma_black_help current_pos t ((x,y)::accu) st
  in 
  match lst with
  |[] -> accu
  |(x,y)::t -> 
    let a = fst current_pos in
    let b = snd current_pos in
    if x >=1 && x <= 9 && y>= 1 && y <= 10 then
      begin 
        if x-a = -2 then helper t x y (a-1) b
        else if x-a = 2 then helper t x y (a+1) b
        else if y-b = 2 then helper t x y a (b+1)
        else helper t x y a (b-1)
      end 
    else ma_black_help current_pos t accu st

(** [next_valid_ma_black current_pos st] is positions the black Ma can move to 
    from current position [current_pos] in state [st]. *)
let next_valid_ma_black current_pos st = 
  let a = fst current_pos in 
  let b = snd current_pos in
  ma_black_help current_pos 
    [(a-2, b+1);(a-2, b-1);(a+2, b+1);(a+2, b-1);(a-1,b+2);(a+1,b+2);(a-1,b-2);
     (a+1,b-2)]
    [] st

(** [next_valid_che_red current_pos st] is positions the red Che can move to 
    from current position [current_pos] in state [st]. *)
let next_valid_che_red current_pos st = 
  (red_move_less_b st current_pos) @ (red_move_more_b st current_pos) 
  @ (red_move_less_a st current_pos) @ (red_move_more_a st current_pos)

(** [next_valid_che_black current_pos st] is positions the black Che can move to 
    from current position [current_pos] in state [st]. *)
(* Functions that will be written during MS2 *)
let next_valid_che_black current_pos st = 
  (black_move_less_b st current_pos) @ (black_move_more_b st current_pos)
  @ (black_move_less_a st current_pos) @ (black_move_more_a st current_pos)

(* Functions that will be written during MS2 *)
let next_valid_pao_red current_pos st = 
  (red_pao_move_less_b st current_pos) @ (red_pao_move_more_b st current_pos)
  @ (red_pao_move_less_a st current_pos) @ (red_pao_move_more_a st current_pos)

(* Functions that will be written during MS2 *)
let next_valid_pao_black current_pos st = 
  (black_pao_move_less_b st current_pos) 
  @ (black_pao_move_more_b st current_pos)
  @ (black_pao_move_less_a st current_pos) 
  @ (black_pao_move_more_a st current_pos)

(** [next_valid_shi current_pos st] is a list of next valid position of black 
    shi. If there are Black color chess in the potential valid position, that 
    position becomes invalid. However, if there are Red color chess in the 
    potential valid position, that position is still valid. 
    Require: current_pos is a valid position, i.e. [pos] is inside Tianzige.  *)
let next_valid_shi_black current_pos st= 
  let black = st.black_remain_chess in 
  let black_pos = pos_lst black in 
  let potential =
    match current_pos with
    |(4,1) -> [(5,2)]
    |(6,1) -> [(5,2)]
    |(5,2) -> [(4,1); (6,1); (4,3);(6,3)]
    |(4,3) -> [(5,2)]
    |(6,3) -> [(5,2)]
    |_ -> failwith "impossible"
  in List.filter (fun elt -> not (List.mem elt black_pos)) potential

(** [next_valid_shi current_pos st] is a list of next valid position of red shi. 
    If there are Red color chess in the potential valid position, that position 
    becomes invalid. However, if there are Black color chess in the potential 
    valid position, that position is still valid. 
    Require: current_pos is a valid position, i.e. [pos] is inside Tianzige.  *)
let next_valid_shi_red current_pos st= 
  let red = st.red_remain_chess in 
  let red_pos = pos_lst red in
  let potential =
    match current_pos with
    |(6,10) -> [(5,9)]
    |(4,10) -> [(5,9)]
    |(5,9) -> [(6,10); (4,10); (6,8); (4,8)]
    |(6,8) -> [(5,9)]
    |(4,8) -> [(5,9)]
    |_ -> failwith "impossible"
  in List.filter (fun elt -> not (List.mem elt red_pos)) potential

(** [next_valid_jiang current_pos st] is a list of next valid position of Jiang. 
    If there are Black color chess in the potential valid position, that 
    position becomes invalid. However, if there are Red color chess in the 
    potential valid position, that position is still valid. 
    Require: current_pos is a valid position, i.e. [pos] is inside Tianzige.  *)
let next_valid_jiang current_pos st = 
  let black = st.black_remain_chess in 
  let black_pos = pos_lst black in 
  let potential = 
    match current_pos with 
    | (5, 1) -> [(6, 1); (4, 1); (5, 2)]
    | (4, 1) -> [(5, 1); (4, 2)]
    | (6, 1) -> [(5, 1); (6, 2)]
    | (5, 2) -> [(5, 1); (5, 3); (4, 2); (6, 2)]
    | (4, 2) -> [(4, 1); (4, 3); (5, 2)]
    | (6, 2) -> [(6, 1); (6, 3); (5, 2)]
    | (5, 3) -> [(5, 2); (4, 3); (6, 3)]
    | (4, 3) -> [(5, 3); (4, 2)]
    | (6, 3) -> [(5, 3); (6, 2)]
    | _ -> failwith "Next Valid Jiang Impossible"
  in List.filter (fun elt -> not (List.mem elt black_pos)) potential 

(** [next_valid_shuai current_pos st] is a list of next valid position of Shuai. 
    If there are Red color chess in the potential valid position, that position 
    becomes invalid. However, if there are Red color chess in the potential 
    valid position, that position is still valid. 
    Require: current_pos is a valid position. [pos] is inside Tianzige.  *)
let next_valid_shuai current_pos st = 
  let red = st.red_remain_chess in 
  let red_pos = pos_lst red in 
  let potential = 
    match current_pos with 
    | (5, 10) -> [(6, 10); (4, 10); (5, 9)]
    | (4, 10) -> [(5, 10); (4, 9)]
    | (6, 10) -> [(5, 10); (6, 9)]
    | (5, 9) -> [(5, 10); (5, 8); (4, 9); (6, 9)]
    | (4, 9) -> [(4, 10); (4, 8); (5, 9)]
    | (6, 9) -> [(6, 10); (6, 8); (5, 9)]
    | (5, 8) -> [(5, 9); (4, 8); (6, 8)]
    | (4, 8) -> [(5, 8); (4, 9)]
    | (6, 8) -> [(5, 8); (6, 9)]
    | _ -> failwith "Next Valid Shuai Impossible"
  in List.filter (fun elt -> not (List.mem elt red_pos)) potential

(** [current_pos color name st] is the current position of the chess with [name] 
    and has color [color]. *)
let current_pos color name st = 
  let rec current_pos_helper name lst = 
    match lst with 
    | [] -> failwith "Impossible current position"
    | h::t -> if fst h = name then snd h else current_pos_helper name t 
  in
  if color = Board.Red 
  then let lst = st.red_remain_chess in current_pos_helper name lst 
  else let lst = st.black_remain_chess in current_pos_helper name lst


(** [check_color name color b] is true if the chess [name] has the same color
    as [color]. False otherwise. 
    Raise [UnknownChess name] if it is not a valid chess name. *)
let check_color name color b= 
  let chess_color = Board.color b name in 
  chess_color = color

(** [modify_remain_chess chess nextpos lst acc] modify the current remain 
    chess if [chess] is not inside current_remain_chess.  *)
let rec modify_remain_chess chess nextpos lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> if fst h =chess 
    then modify_remain_chess chess nextpos t ((chess, nextpos)::acc)
    else  modify_remain_chess chess nextpos t (h::acc)

(** [delete_chess chess nextpos lst acc] delete current remain chess if [chess]
    is inside the current_remain_chess. *)
let rec delete_chess chess nextpos lst acc = 
  match lst with 
  | [] -> acc
  | h::t -> if snd h=nextpos 
    then delete_chess chess nextpos t acc
    else delete_chess chess nextpos t (h::acc)

(** [update_state color chess nextpos st] is [st']. If there is a chess [ch2] 
    with different color on [pos], [ch2] is deleted from the color1_remain_chess 
    list. Then, we update the color2_remain_chess. Specifically, the pos of 
    [chess] is changed into a new position on the board. 
    Require: [pos] is a valid position for [chess]. *)
let update_state color chess nextpos st = 
  let red = st.red_remain_chess in 
  let black = st.black_remain_chess in
  if color = Board.Red 
  then let red_remain_new = modify_remain_chess chess nextpos red [] in 
    let black_remain_new = delete_chess chess nextpos black [] in 
    {red_remain_chess = red_remain_new ; black_remain_chess = black_remain_new}
  else let black_remain_new = modify_remain_chess chess nextpos black [] in 
    let red_remain_new = delete_chess chess nextpos red [] in 
    {red_remain_chess = red_remain_new ; black_remain_chess = black_remain_new }

let current_red_captured st = 
  let all_red = [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); 
                 ("Bing", 5); ("Shi", 1); ("Shi", 2); ("Xiang", 1); 
                 ("Xiang", 2); ("Ma", 1); ("Ma", 2); ("Pao", 1); ("Pao", 2); 
                 ("Che", 1); ("Che", 2); ("Shuai", 1)] in
  let red = List.map (fun ((name, id), (_, _))
                       -> (name, id)) st.red_remain_chess in 
  List.filter (fun (name, id) -> not (List.mem (name, id) red)) all_red

let current_black_captured st = 
  let all_black = [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); 
                   ("Zu", 5); ("Shi", 3); ("Shi", 4); ("Xiang", 3); 
                   ("Xiang", 4); ("Ma", 3); ("Ma", 4); ("Pao", 3); ("Pao", 4); 
                   ("Che", 3); ("Che", 4); ("Jiang", 1)] in
  let black = List.map (fun ((name, id), (_, _)) 
                         -> (name, id)) st.black_remain_chess in 
  List.filter (fun (name, id) -> not (List.mem (name, id) black)) all_black

(** [check_color_name c name st] raises [InvalidChess error_msg] if [name] is a 
    different color chess compare with [c]; if [name] is an already captured 
    chess; if [name] is an already captured chess and has different color than 
    [c].  *)
let check_color_name c name st = 
  let remain_red = List.map (fun ((name, id), (pos1, pos2)) 
                              -> (name, id)) st.red_remain_chess in 
  let remain_black = List.map (fun ((name, id), (pos1, pos2)) 
                                -> (name, id)) st.black_remain_chess in
  let captured_red = current_red_captured st in 
  let captured_black = current_black_captured st in 
  if c = Board.Red 
  then begin 
    if List.mem name remain_black then raise (InvalidChess "You are attempting to move a black chess. Please select a red chess. ")
    else if List.mem name captured_red then raise (InvalidChess "You are attempting to move a red chess that has already been captured. Please select a red remain chess. ")
    else if List.mem name captured_black then raise (InvalidChess "You are attempting to move a black chess that has already been captured. Please select a red chess. ")
    else ()
  end 
  else begin 
    if List.mem name remain_red then raise (InvalidChess "You are attempting to move a red chess. Please select a black chess. ")
    else if List.mem name captured_black then raise (InvalidChess "You are attempting to move a black chess that has already been captured. Please select a black remain chess. ")
    else if List.mem name captured_red then raise (InvalidChess "You are attempting to move a red chess that has already been captured. Please select a black chess. ")
    else ()
  end

(** [move_new_state nextpos name color st f] is [r] if attempting to move a 
    chess [name] with color [color] to [nextpos] given that name is valid. 
    [f] is a function that gives the valid position of the related chess. If 
    the [nextpos] is a valid position, it is [Legal st']. If the [nextpos] is 
    not a valid position, it is [Illegal].
    This is a helper function for [move_red nextpos name st] and 
    [move_black next_pos name st]. *)
let move_new_state nextpos name color st f = 
  let cpos = current_pos color name st in 
  let valid_pos = f cpos st in
  if List.mem nextpos valid_pos 
  then Legal (update_state color name nextpos st) 
  else Illegal 

(** [move_red nextpos name st] is [r] if attempting to move a chess [name] 
    with color Red to [nextpos]. If the [name] is not a valid chess name, it is 
    [Illegal]. If the [nextpos] is not a valid next position, it is [Illegal].
    Otherwise, it is [Legal st']. 
    This is a helper function for [move nextpos name color b st] *)
let move_red nextpos name st = 
  if (fst name = "Bing") && (snd name = 1 || snd name = 2 || snd name = 3 || 
                             snd name = 4 || snd name =5)  
  then move_new_state nextpos name Board.Red st next_valid_bing
  else if (name = ("Shi", 1) || name = ("Shi", 2))
  then move_new_state nextpos name Board.Red st next_valid_shi_red
  else if name = ("Shuai", 1)
  then move_new_state nextpos name Board.Red st next_valid_shuai
  else if (name = ("Xiang", 1) || name = ("Xiang", 2))
  then move_new_state nextpos name Board.Red st next_valid_xiang_red
  else if (name = ("Ma", 1)||name = ("Ma", 2))
  then move_new_state nextpos name Board.Red st next_valid_ma_red
  else if (name = ("Che", 1) || name = ("Che", 2))
  then move_new_state nextpos name Board.Red st next_valid_che_red
  else if (name = ("Pao", 1) || name = ("Pao", 2))
  then move_new_state nextpos name Board.Red st next_valid_pao_red
  else Illegal

(** [move_black nextpos name st] is [r] if attempting to move a chess [name] 
    with color Black to [nextpos]. If the [name] is not a valid chess name, it 
    is [Illegal]. If the [nextpos] is not a valid next position, it is 
    [Illegal]. Otherwise, it is [Legal st']. 
    This is a helper function for [move nextpos name color b st] *)
let move_black nextpos name st = 
  if (fst name = "Zu" && (snd name = 1 || snd name = 2 || snd name = 3 || 
                          snd name = 4 || snd name =5) )
  then move_new_state nextpos name Board.Black st next_valid_zu
  else if (name = ("Shi", 3) || name = ("Shi", 4))
  then move_new_state nextpos name Board.Black st next_valid_shi_black
  else if name = ("Jiang", 1) 
  then move_new_state nextpos name Board.Black st next_valid_jiang
  else if (name = ("Xiang", 3) || name = ("Xiang", 4))
  then move_new_state nextpos name Board.Black st next_valid_xiang_black
  else if (name = ("Ma", 3) || name = ("Ma", 4))
  then move_new_state nextpos name Board.Black st next_valid_ma_black
  else if (name = ("Che", 3) || name = ("Che", 4))
  then move_new_state nextpos name Board.Black st next_valid_che_black
  else if (name = ("Pao", 3) || name = ("Pao", 4))
  then move_new_state nextpos name Board.Black st next_valid_pao_black
  else Illegal 

let move nextpos name color b st =
  check_color_name color name st;
  if color = Board.Red then move_red nextpos name st 
  else move_black nextpos name st 
