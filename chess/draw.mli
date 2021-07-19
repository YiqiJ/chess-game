(** 
   Print Chinese Chess Board in Terminal. 
*)

(** [print_board b st] if [b] is true, prints the Chinese Chess Board due 
    to the current state [st]. if [b] is false, prints nothing. *)
val print_board : bool -> State.t -> unit