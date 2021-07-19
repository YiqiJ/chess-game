(**
   Parsing of player commands.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["move shuai-1 to (2,2)"], then the object phrase 
      is [("shuai-1", (2,2))].
    - If the player command is ["move shuai-1     to (4,4)"], then the object 
      phrase is again [("shuai-1", (4,4))]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = (string * int) * (int * int)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Move of object_phrase
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** Raised when the user input are not in correct format but can be decomposed 
    into valid chess name, location, row, and col.  *)
exception DoYouMean of (string * (Board.name * (int * int)))

(** Raised when there are non-empty object-phrase follow the verb "quit". *)
exception DoYouQuit

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    move   shuai-1   to  (3,5) "] is [Move ("shuai-1", (3,5))]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9), -,  and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed msg] if 
    1. the command is malformed. A command
       is {i malformed} 
    2. the verb is neither "quit" nor "move",
    3. the verb is "move" and there is an empty object phrase.
    4. after decompose, chess name is not a valid chess name
    5. after decompose, chess index is not a valid index
    6. after decompose, next location does not have valid [col] and [row]
    where [msg] is helper message that will be displayed according to the 
    malformed situation.

    Raises: [DoYouMean (name, next location)] if the user input format is not 
    strictly followes [move <chess_name>-<index> to <(col, row)>] but 
    [chess_name], [index], [col], and [row] are all valid. 

    Raises: [DoYouQuit] if the verb is "quit" and there are non-empty 
    object phrase.
*)
val parse : string -> command

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)