(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = (string * int) * (int * int)

type command = 
  | Move of object_phrase
  | Quit

exception Empty

exception Malformed of string
exception DoYouMean of (string * (Board.name * (int * int)))
exception DoYouQuit

exception DoYouMeanChessName of Board.name
exception DoYouMeanChessLocation of (int * int)


(** [form_valid_list acc lst] delete all the empty string element in a given 
    list. *)
let rec form_valid_list acc = function
  | [] -> acc
  | h::t -> if h="" then (form_valid_list acc t) else form_valid_list (h::acc) t

(** [is_int s] is true if [s] only contains digits. False otherwise. *)
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

(** [is_valid_name name num] is (lower case name, id) if name is a valid chess 
    name and num is an integer. 
    Raises: 
    1. [Malformed "Name should be followed by an integer index. "] if num is not
       an integer. 
    2. [Malformed "Invalid chess name. Valid chess names are Bing, Zu, Che, Ma, 
       Pao, Shi, Xiang, Shuai, Jiang. "] if name is not a valid chess name. *)
let is_valid_name name num = 
  let valid_chess_name = ["bing"; "zu"; "shi"; "xiang"; "ma"; "jiang"; "shuai"; 
                          "che"; "pao"] in 
  let low_name = String.lowercase_ascii name in 
  if not (is_int num) 
  then raise (Malformed "Name should be followed by an integer index. ")
  else if not (List.mem low_name valid_chess_name) 
  then  raise (Malformed "Invalid chess name. Valid chess names are Bing, Zu, Che, Ma, Pao, Shi, Xiang, Shuai, Jiang. ")
  else (String.capitalize_ascii low_name, int_of_string num)

(** [is_valid_namenum namenum] raises [DoYouMeanChessName (name, id)] if 
    [namenum] can be decomposed into valid chess name and valid index. By 
    decompose, we mean the last [namenum] character represents index and other 
    characters togther represents chess name. *)
let is_valid_namenum namenum = 
  let len = String.length namenum in 
  let name = String.sub namenum 0 (len-1) in 
  let num = String.sub namenum (len-1) 1 in 
  match is_valid_name name num with 
  | (chess, id) -> raise (DoYouMeanChessName (chess,id)) 

(** [chess_name t] is the (name, id) of the chess if the input is valid.
    Raises: 
    1. [DoYouMean (name, id)] if [t] is not valid but can be decomposed into 
    valid name and index. 
    2. [Malformed "Command should be given as <chessname-index> to 
    (pos1, pos2)."] if [t] is not valid and cannot be decomposed into valid name
    and index.  *)
let chess_name t = 
  match t with 
  | h::"to"::t -> let lst = String.split_on_char '-' h in
    begin 
      match lst with 
      | name::num::[] -> is_valid_name name num
      | namenum::[] -> is_valid_namenum namenum
      | _ -> raise (Malformed "Command should be given as <chessname-index> to (pos1, pos2).")
    end
  | n::i::"to"::t -> 
    begin 
      let name = String.split_on_char '-' n |> String.concat "" in 
      let num = String.split_on_char '-' i |> String.concat "" in 
      is_valid_namenum (name^num)
    end 
  | n::"-"::i::t -> is_valid_namenum (n^i)
  | _ -> if List.mem "to" t then raise (Malformed "Command should be given as <chessname-index> to (pos1, pos2). Your keyword 'to' is in the wrong place. ")
    else raise (Malformed "Command should be given as <chessname-index> to (pos1, pos2). You are missing a keyword 'to'. ")

(** [col_row_int fst snd] is [(col, row)] if [fst] is integer between 0 and 9
    and [snd] is integer between 0 and 10. 

    Raises [Malformed msg] if either [fst] or [snd] is not integer. [msg] is 
    helper message for a specific condition *)
let col_row_int fst snd = 
  let first = int_of_string fst in 
  if first >= 10 || first <= 0
  then raise (Malformed "Location (col, row), col should be integer between 1 and 9. ")
  else let second = int_of_string snd in 
    begin 
      if second >=11 || second <= 0 
      then raise (Malformed "Location (col, row), row should be integer between 1 and 10. ")
      else (int_of_string fst, int_of_string snd)
    end 

(** [miss_right_and_left_paranthesis s] is (col, row) if given that [s] misses 
    both right and left parenthesis and can be decomposed into valid (col, row), 
    where [col] is an integer between 0 and 9 and [row] is an integer between 
    0 and 10. By decompose we mean characters on the left side of comma is [col]
    and characters on the right side of comma is [row].

    Raises [Malformed msg] if 
    1. [s] does not contain a comma. 
    2. either [col] or [row] is not an integer.
    3. [col] is not an integer between 1 and 9. 
    4. [row] is not an integer between 1 and 10. 
    where [msg] is helper message that specify which the above four situations 
    it encounters.
    This is a helper function for [next_location_helper t] *)
let miss_rght_lft_parenthesis s = 
  if not (String.contains s ',')
  then raise (Malformed "Location should be written as (col, row). You might miss a comma. ")
  else begin 
    let comma = String.index s ',' in
    let fst = String.sub s 0 comma in
    let snd = String.sub s (comma+1) ((String.length s)-comma-1) in
    if not ((is_int fst) && (is_int snd)) 
    then raise (Malformed "Location should be written as (col, row). Your col or row might not be an integer. ")
    else col_row_int fst snd
  end 

(** [miss_left_paranthesis s] is (col, row) if given that [s] misses 
    only left parenthesis and can be decomposed into valid (col, row), 
    where [col] is an integer between 0 and 9 and [row] is an integer between 
    0 and 10. By decompose we mean characters on the left side of comma is [col]
    and characters on the right side between comma and right parenthesis is 
    [row].

    Raises [Malformed msg] if 
    1. [s] does not contain a comma. 
    2. either [col] or [row] is not an integer.
    3. [col] is not an integer between 1 and 9. 
    4. [row] is not an integer between 1 and 10. 
    where [msg] is helper message that specify which the above four situations 
    it encounters.
    This is a helper function for [next_location_helper t] *)
let miss_left_paranthesis s =
  let str = String.sub s 0 (String.index s ')') in 
  miss_rght_lft_parenthesis str

(** [miss_right_paranthesis s] is (col, row) if given that [s] misses 
    only left parenthesis and can be decomposed into valid (col, row), 
    where [col] is an integer between 0 and 9 and [row] is an integer between 
    0 and 10. By decompose we mean characters between the left parenthesis and
    comma is [col] and characters on the right side of the comma is [row].

    Raises [Malformed msg] if 
    1. [s] does not contain a comma. 
    2. either [col] or [row] is not an integer.
    3. [col] is not an integer between 1 and 9. 
    4. [row] is not an integer between 1 and 10. 
    where [msg] is a helper message that specify which the above four situations 
    it encounters. 
    This is a helper function for [next_location_helper t] *)
let miss_right_paranthesis s =
  let str = String.sub s ((String.index s '(' )+1) (String.length s -1) in
  miss_rght_lft_parenthesis str

(** [next_location_helper t] is the next location if [t] contains both 
    parenthesis, comma, col, and row. 

    Raises [DoYouMeanChessLocation (col, row)] if 
    1. [t] is missing one or both of the parenthesis but can be decomposed into
       a valid next location. By decompose we mean the characters on the left of 
       the comma is valid [col] and the characters on the right of the comma is 
       valid [row]

    Raises [Malformed msg] if 
    1. [t] does not contains a comma
    2. After decomposing, [row] and [col] are not valid numbers. 
    This is a helper function for [next_location t]. *)
let next_location_helper t = 
  let s = String.concat "" t in 
  if not (String.contains s '(') && not (String.contains s ')') then
    match miss_rght_lft_parenthesis s with
    |(h,t) -> raise (DoYouMeanChessLocation(h,t)) 
  else if not (String.contains s '(') then
    match miss_left_paranthesis s with 
    |(h,t) -> raise (DoYouMeanChessLocation(h,t))
  else if not (String.contains s ')') then
    match miss_right_paranthesis s with
    |(h,t) -> raise (DoYouMeanChessLocation (h,t))
  else s 

(** [next_location t] is the next location (col, row) if [t] can be decomposed 
    into a valid location. By decompose we mean in [t] there is left 
    parenthesis, right parenthesis, comma, an integer between left parenthesis
    and comma, and an integer between comma and right parenthesis. 

    Raises [Malformed msg] if 
    1. [t] does not contain comma.
    2. characerts on the left of the comma or the right of the comma is not 
       integer.
    3. [col] is not an integer between 1 and 9.
    4. [row] is not an integer between 1 and 10.
    where msg is a helper message that specify which the above four situations 
    it encounters.

    Raises [DoYouMeanChessLocation (col, row)] if 
    1. [t] is missing one or both of the parenthesis but may be a correct 
       location.
    but both [col] and [row] are valid numbers.  *)
let next_location t = 
  let rec helper t = 
    match t with 
    | [] -> [] 
    | h::t -> if h="to" then t else helper t
  in 
  let after_to = helper t in 
  let str = next_location_helper after_to in 
  let lft = String.index str '(' in 
  let rgt = String.index str ')' in 
  let s = String.sub str (lft+1) (rgt-lft-1) in 
  if not (String.contains s ',') 
  then raise (Malformed "Location should be written as (col, row). You might miss a comma. ")
  else let comma = String.index s ',' in 
    let fst = String.sub s 0 comma in 
    let snd = String.sub s (comma+1) ((String.length s)-comma-1) in 
    if not ((is_int fst) && (is_int snd)) 
    then raise (Malformed "Location should be written as (col, row). Your col or row might not be an integer. ")
    else col_row_int fst snd

(** [parse_print chess id col row] is the message that will be displayed and the
    possible chess name [chess], possible index [id], possible col [col], and 
    possible row [row] that the player want to move to. *)
let parse_print chess id col row = 
  (("Did you mean move "^chess^"-"^(string_of_int id)^" to ("^
    (string_of_int col)^", "^
    (string_of_int row)^") ? [y/N]"), ((chess, id), (col, row)))

(** [parse_name_helper t name] parses a player's input into a [command] 
    [Move (name, position)] given that [name] is valid chess name and 
    if the location is valid. 

    Raises: [DoYouMean (msg, (name, location))] if location might be a valid 
    location. 

    Raises: [Malformed msg] if 
    1. [t] does not contain comma.
    2. After decomposing [t], [col] and [row] are not valid number. 
    [col] is the characters on the left side of comma without the left 
    parenthesis; [row] is the characters on the right side of comma without the 
    right parenthesis.
    This is a helper function for [parse str] *)
let parse_name_helper t chess_name = 
  match chess_name with 
  | (chess, id) -> 
    begin 
      try let nextLocation = next_location t in 
        begin 
          match nextLocation with 
          | (col, row) -> Move (chess_name, nextLocation)
        end 
      with        
      | DoYouMeanChessLocation (col, row) -> 
        raise (DoYouMean (parse_print chess id col row))  
    end 

(** [parse_nextloc_helper t chess id] parses a player's input into a [command]
    given that the chess name is not in the correct form but might be valid. 

    Raises [DoYouMean (name, position)] if 
    1. next location can be parsed out correctly.
    2. next location cannot be parsed out correctly, but might be valid. 

    Raises [Malformed msg] if 
    1. [t] does not contain comma.
    2. After decomposing [t], [col] and [row] are not valid number.
    This is a helper function for [parse str]
*)
let parse_nextloc_helper t chess id = 
  try let nextLocation = next_location t in 
    match nextLocation with 
    | (col, row) -> 
      raise (DoYouMean (parse_print chess id col row))
  with 
  | DoYouMeanChessLocation (col, row) -> 
    raise (DoYouMean (parse_print chess id col row))

let parse str = 
  let splitlst = String.split_on_char ' ' str in  
  let oblst = (List.rev (form_valid_list [] splitlst)) in 
  match oblst with 
  |  [] -> raise Empty
  |  h::t -> if  h="quit" && t=[] then Quit
    else if h="move" && t<>[] then 
      begin 
        try let chessName = chess_name t in 
          parse_name_helper t chessName
        with       
        | DoYouMeanChessName (chess, id) -> 
          parse_nextloc_helper t chess id
      end 
    else if h="quit" && t<>[] then raise DoYouQuit
    else if h="move" && t=[] then raise (Malformed "Your command 'move' should be followed by <chess name>-<index> to (col, row). ")
    else raise (Malformed "Your command need to start with move or quit. ")

