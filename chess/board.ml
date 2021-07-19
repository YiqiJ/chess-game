type color = Red | Black

type name = string * int

type point = int * int

type position = point

(*type board = point * point * point * point*)

exception UnknownChess of name
exception InvalidPos of position

type chess =
  {
    color : color;
    name : string * int;
    initial_pos : point; 
  }

type t = {chesses : chess list}

(** [r_shuai1] is the red chess shuai-1. *)
let r_shuai1 = 
  {
    color = Red;
    name = ("Shuai", 1);
    initial_pos = (5, 10)
  }

(** [r_che1] is the red chess che-1. *)
let r_che1 = 
  {
    color = Red;
    name = ("Che", 1);
    initial_pos = (1, 10)
  }

(** [r_che2] is the red chess che-2. *)
let r_che2 = 
  {
    color = Red;
    name = ("Che", 2);
    initial_pos = (9, 10)
  }

(** [r_pao1] is the red chess pao-1. *)
let r_pao1 = 
  {
    color = Red;
    name = ("Pao", 1);
    initial_pos = (2, 8)
  }

(** [r_pao2] is the red chess pao-2. *)
let r_pao2 = 
  {
    color = Red;
    name = ("Pao", 2);
    initial_pos = (8, 8)
  }

(** [r_ma1] is the red chess ma-1. *)
let r_ma1 = 
  {
    color = Red;
    name = ("Ma", 1);
    initial_pos = (2, 10)
  }

(** [r_ma2] is the red chess ma-2. *)
let r_ma2 = 
  {
    color = Red;
    name = ("Ma", 2);
    initial_pos = (8, 10)
  }

(** [r_xiang1] is the red chess xiang-1. *)
let r_xiang1 = 
  {
    color = Red;
    name = ("Xiang", 1);
    initial_pos = (3, 10)
  }

(** [r_xiang2] is the red chess xiang-2. *)
let r_xiang2 = 
  {
    color = Red;
    name = ("Xiang", 2);
    initial_pos = (7, 10)
  }

(** [r_shi1] is the red chess shi-1. *)
let r_shi1 = 
  {
    color = Red;
    name = ("Shi", 1);
    initial_pos = (4, 10)
  }

(** [r_shi2] is the red chess shi2. *)
let r_shi2 = 
  {
    color = Red;
    name = ("Shi", 2);
    initial_pos = (6, 10)
  }

(** [bing1] is the red chess bing-1. *)
let bing1 = 
  {
    color = Red;
    name = ("Bing", 1);
    initial_pos = (1, 7)
  }

(** [bing2] is the red chess bing2. *)
let bing2 = 
  {
    color = Red;
    name = ("Bing", 2);
    initial_pos = (3, 7)
  }

(** [bing3] is the red chess bing3. *)
let bing3 = 
  {
    color = Red;
    name = ("Bing", 3);
    initial_pos = (5, 7)
  }

(** [bing4] is the red chess bing4. *)
let bing4 = 
  {
    color = Red;
    name = ("Bing", 4);
    initial_pos = (7, 7)
  }

(** [bing5] is the red chess bing5. *)
let bing5 = 
  {
    color = Red;
    name = ("Bing", 5);
    initial_pos = (9, 7)
  }

(** [b_jiang1] is the black chess jiang-1. *)
let b_jiang1 = 
  {
    color = Black;
    name = ("Jiang", 1);
    initial_pos = (5, 1)
  }

(** [b_che1] is the black chess che-1. *)
let b_che1 = 
  {
    color = Black;
    name = ("Che", 3);
    initial_pos = (1, 1)
  }

(** [b_che2] is the black chess che-2. *)
let b_che2 = 
  {
    color = Black;
    name = ("Che", 4);
    initial_pos = (9, 1)
  }

(** [b_pao1] is the black chess pao-1. *)
let b_pao1 = 
  {
    color = Black;
    name = ("Pao", 3);
    initial_pos = (2, 3)
  }

(** [b_pao2] is the black chess pao-2. *)
let b_pao2 = 
  {
    color = Black;
    name = ("Pao", 4);
    initial_pos = (8, 3)
  }

(** [b_ma1] is the black chess ma-1. *)
let b_ma1 = 
  {
    color = Black;
    name = ("Ma", 3);
    initial_pos = (2, 1)
  }

(** [b_ma2] is the black chess ma-2. *)
let b_ma2 = 
  {
    color = Black;
    name = ("Ma", 4);
    initial_pos = (8, 1)
  }

(** [b_xiang1] is the black chess xiang-1. *)
let b_xiang1 = 
  {
    color = Black;
    name = ("Xiang", 3);
    initial_pos = (3, 1)
  }

(** [b_xiang2] is the black chess xiang-2. *)
let b_xiang2 = 
  {
    color = Black;
    name = ("Xiang", 4);
    initial_pos = (7, 1)
  }

(** [b_shi1] is the black chess shi-1. *)
let b_shi1 = 
  {
    color = Black;
    name = ("Shi", 3);
    initial_pos = (4, 1)
  }

(** [b_shi2] is the black chess shi-2. *)
let b_shi2 = 
  {
    color = Black;
    name = ("Shi", 4);
    initial_pos = (6, 1)
  }

(** [zu1] is the black chess zu-1. *)
let zu1 = 
  {
    color = Black;
    name = ("Zu", 1);
    initial_pos = (1, 4)
  }

(** [zu2] is the black chess zu-2. *)
let zu2 = 
  {
    color = Black;
    name = ("Zu", 2);
    initial_pos = (3, 4)
  }

(** [zu3] is the black chess zu-3. *)
let zu3 = 
  {
    color = Black;
    name = ("Zu", 3);
    initial_pos = (5, 4)
  }

(** [zu4] is the black chess zu-4. *)
let zu4 = 
  {
    color = Black;
    name = ("Zu", 4);
    initial_pos = (7, 4)
  }

(** [zu5] is the black chess zu-5. *)
let zu5 = 
  {
    color = Black;
    name = ("Zu", 5);
    initial_pos = (9, 4)
  }

let init_board =
  let qizi = [r_shuai1; r_che1; r_che2; r_pao1; r_pao2; r_ma1; r_ma2; r_xiang1; 
              r_xiang2; r_shi1; r_shi2; bing1; bing2; bing3; bing4; 
              bing5; b_jiang1; b_che1; b_che2; b_pao1; b_pao2; b_ma1; b_ma2;
              b_xiang1; b_xiang2; b_shi1; b_shi2; zu1; zu2; zu3; zu4; zu5] in 
  {chesses = qizi}

let chesses b = b.chesses

(** [find_chess b chess] is the chess with name [chess] in board [b]. 
    Raises: UnknownChess if [chess] is not a valid chess in [b]. *)
let rec find_chess b chess =
  try List.find (fun (x:chess) -> x.name = chess ) b.chesses
  with Not_found -> raise (UnknownChess chess)

let color b name = 
  let ch = find_chess b name in ch.color

let color_chess chess = chess.color

let name_chess chess = chess.name 

let pos_chess chess = chess.initial_pos