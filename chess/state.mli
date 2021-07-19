(** The abstract type of values representing the game state. *)
type t 

(** [init_state a] is the initial state of the game, i.e. chesses are all at 
    their original position.  *)
val init_state : t

(** [red_remain_chess st] is a set-like list of the current remain red 
    chess pieces on board at state [st].  *)
val red_remain_chess : t -> (Board.name * (int * int)) list

(** [red_remain_chess st] is a set-like list of the current remain black 
    chess pieces on board at state [st].  *)
val black_remain_chess : t -> (Board.name * (int * int)) list

(** [current_red_captured st] is the current red chesses that have been captured 
    by the opponent - black player. *)
val current_red_captured : t -> (Board.name) list

(** [current_black_captured st] is the current black chesses that have been 
    captured by the opponent - red player. *)
val current_black_captured: t -> (Board.name) list

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** Raised when chess is invalid.  *)
exception InvalidChess of string

(** [move nextpos name c st] is [r] if attempting to move a chess [name] 
    with color [color] to [nextpos]. If the chess [name] does not have the 
    same color [c], then it is [Illegal]. If the [pos] is an invalid next 
    position for chess [name], then it is [Illegal]. Otherwise, the result 
    is [Legal st'], where in [st'] the remaining red chess and black chess 
    might change due to some chess eaten by chess [name]. 
    Effects: none.  [move] is not permitted to do any printing. *)
val move : int * int -> string * int -> Board.color ->  Board.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)