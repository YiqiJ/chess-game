(** 
   Representation of static board data.

   This module represents the data of initial Chinese chess board.
   as querying the data.
*)

(** The abstract type of values representing chess boards. *)
type t

(** The type of chess *)
type chess

(** The type of colors. *)
type color = Red | Black

(** The type of chess names *)
type name = string * int

(** The type of positions. *)
type position = int * int

(** Raised when an unknown chess is encountered. *)
exception UnknownChess of name

(** Raised when an invalid position is encountered. *)
exception InvalidPos of position

(** [color b n] is the color of chess [n] in board [b]. *)
val color : t -> name -> color

(** [init_board] is the initial chess borad in playing the game. It contains the
    valid rows and columns allowed in the chess game, all valid existing chesses
    that could be moved, and boundaries set for red and black chesses 
    respectively. *)
val init_board: t

(** [chess b] is the list of chess pieces on board [b] *)
val chesses : t -> chess list

(** [color_chess c] is the color of chess [c]. *)
val color_chess : chess -> color

(** [name_chess c] is the name of chess [c]. *)
val name_chess : chess -> name 

(** [pos_chess c] is the position of chess [c]. *)
val pos_chess : chess -> position

