(** Our test cases using OUnit test for the validity of the initial board, the 
    parse functionality which parses player's input commanding to move the 
    chesses and the movements of all types of chesses according the the Chinese 
    chess rules. The board display (the board printed on terminal and updated 
    after every movement), the winning condition are tested manually by make
    play, the parsing of possible valid input of user, and the response of user 
    for "Did you mean" question if their input might possibly be valid. We used 
    glass box testing in writing thorough test cases for modules
    Board, Command and State. Our testing approach is developed as such that 
    we test for every potential position (every general direction that is 
    cosidered as different condition in our implementation) that matches each 
    line in our implementation and test for positions that each chess cannot 
    move under certain circumstances according to the rule. We believe that the 
    test demonstrate the correctness of our gaming system as our test cases 
    covered circumstances (for each type of chess in red and black) which 
    1. the chess can move validly in its initial position; 
    2. all positions the chess can validly move to in its current
    position when its path is not "blocked" -- meaning it can move to all its
    potential position by rule without other chesses's interference; 
    3. the chess cannot move to certain positions (where it originally can if 
    not blocked) if its path is blocked by some other chess (both blocked by red
    chess and by black chess)
    4. the chess can "eat" chess with opposite color on its intended moving 
    position (if it can move validly to according to the rule), and cannot move 
    to such location if it is occupied by another chess with the same color.
    5. the chess can not move across certain boundaries, and the boundaries 
    differ for different types of chess. *)
open OUnit2
open Board
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [print_state st] is a pretty-prints state [st] *)
let print_state lst = 
  let rec helper lst = 
    match lst with 
    | [] -> ""
    | ((name, id), (a, b)) ::t -> name^"-"^(string_of_int id)^" ("^
                                  (string_of_int a)^", "^(string_of_int b)^
                                  ") ; "^helper t 
  in 
  helper lst 

(** [print_name st] is a pretty-prints (chess_name, id) of state [st]. *)
let print_name lst = 
  let rec helper lst = 
    match lst with 
    | [] -> ""
    | (name, id)::t -> "("^name^"-"^(string_of_int id)^"); "^helper t 
  in 
  helper lst

(** [print_command cmd] is a pretty-prints command [cmd]. *)
let print_command cmd = 
  match cmd with 
  | Quit -> "quit"
  | Move ((name, id), (a, b)) -> name^"-"^(string_of_int id)^" ("^
                                 (string_of_int a)^", "^(string_of_int b)^
                                 ")"

(** [multi_move nextposlst namelst colorlst b st] is the ultimate state of 
    successive moves. [nextposlst] is a list of next postion that you want to 
    move. Each position correspond to a name, which is an element inside 
    [namelst] and this element has the same index as pos in [nextposlst], and 
    color, which is an element inside [colorlst] and this element has the same 
    index as pos in [nextposlst]. 
    Require: 
    1. [nextposlst] [namelst] [colorlst] has same length.
    2. Each combination of three lists define a valid move, i.e., it is a [Legal
        state], not an [Illegal] state *)
let multi_move nplst nlst clst b st = 
  let rec helper nplst nlst clst st = 
    match nplst, nlst, clst with 
    | [], [], [] -> st
    | [], _, _ -> failwith "should not match to this line 1"
    | _, [], _ -> failwith "should not match to this line 2"
    | _, _, [] -> failwith "should not match to this line 3"
    | nextpos::pt, name::nt, color::ct -> 
      begin 
        match State.move nextpos name color b st with 
        | Legal newst -> helper pt nt ct newst 
        | Illegal -> failwith "should not match to this line 4"
      end 
  in 
  helper nplst nlst clst st

(********************************************************************
   End helper functions.
 ********************************************************************)

let b = Board.init_board

let ini_st = State.init_state

let make_color_test 
    (name : string)
    (board : Board.t)
    (chess : Board.name)
    (expected_output : Board.color) : test = 
  name >:: (fun _-> 
      assert_equal expected_output (Board.color board chess))

let board_tests =
  [
    make_color_test "Test for valid name zu" b ("Zu", 1) Black;
    make_color_test "Test for valid name shi black" b ("Shi", 3) Black;
    make_color_test "Test for valid name shi red" b ("Shi", 1) Red;
    make_color_test "Test for valid name xiang black" b ("Xiang", 3) Black;
    make_color_test "Test for valid name xiang red" b ("Xiang", 2) Red;
    make_color_test "Test for valid name ma black" b ("Ma", 4) Black;
    make_color_test "Test for valid name ma red" b ("Ma", 2) Red;
    make_color_test "Test for valid name par black" b ("Pao", 3) Black;
    make_color_test "Test for valid name par red" b ("Pao", 2) Red;
    make_color_test "Test for valid name che black" b ("Che", 3) Black;
    make_color_test "Test for valid name che red" b ("Che", 1) Red;
    make_color_test "Test for valid name jiang" b ("Jiang", 1) Black;
    make_color_test "Test for valid name bing" b ("Bing", 2) Red;
    make_color_test "Test for valid name shuai" b ("Shuai", 1) Red;
    "Test for invalid name">::
    (fun _ -> assert_raises (Board.UnknownChess ("Name", 2)) 
        (fun () -> Board.color b ("Name", 2)));
    "Test for invalid name">::
    (fun _ -> assert_raises (Board.UnknownChess ("shuai", 2)) 
        (fun () -> Board.color b ("shuai", 2)));
  ]

let make_parse_test 
    (name : string)
    (str : string)
    (expected_output : Command.command) : test = 
  name >:: (fun _-> 
      assert_equal expected_output (Command.parse str)
        ~printer:print_command)

let command_tests =
  [
    make_parse_test "Test for valid command with no extra space" 
      "move shuai-1 to (1,4)" (Move (("Shuai", 1), (1, 4)));
    make_parse_test "Test for valid command but extra space between" 
      "move     shuai-1     to    (  1  , 6 )" (Move (("Shuai", 1), (1, 6)));
    make_parse_test "Test for valid command but extra space between" 
      "move     Xiang-4     to    (  1  , 6 )" (Move (("Xiang", 4), (1, 6)));
    make_parse_test "Test for valid command jiang-1 (5, 2)" 
      "move jiang-1 to (5, 2)" (Move (("Jiang", 1), (5, 2)));
    make_parse_test "test for quit" "quit" Command.Quit;

    "Test for no key word to">::
    (fun _ -> assert_raises 
        (Command.Malformed "Command should be given as <chessname-index> to (pos1, pos2). You are missing a keyword 'to'. ") 
        (fun () -> Command.parse "move shuai-1 (1,2)"));
    "Test for no move">::
    (fun _ -> assert_raises 
        (Command.Malformed "Your command need to start with move or quit. ") 
        (fun () -> Command.parse " shuai-1 to (1,2)"));

    "Test for have - not in correct place, e.g. Shuai -1"
    >::(fun _ -> assert_raises 
           (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                               (("Shuai", 1), (1, 2)))) 
           (fun () -> Command.parse "move shuai -1 to (1,2)"));

    "Test for have - but not in correct place, e.g. Shuai- 1"
    >::(fun _ -> assert_raises 
           (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                               (("Shuai", 1), (1, 2)))) 
           (fun () -> Command.parse "move shuai- 1 to (1,2)"));

    "Test for have - but not in correct place, e.g. Shuai - 1"
    >::(fun _ -> assert_raises 
           (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                               (("Shuai", 1), (1, 2)))) 
           (fun () -> Command.parse "move shuai - 1 to (1,2)"));

    "Test for no - and chess name is followed immediately by index, e.g. Shuai1"
    >::(fun _ -> assert_raises 
           (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                               (("Shuai", 1), (1, 2)))) 
           (fun () -> Command.parse "move shuai1 to (1,2)"));

    "Test for no - and there is a space between name and index, e.g. Shuai 1"
    >::(fun _ -> assert_raises 
           (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                               (("Shuai", 1), (1, 2)))) 
           (fun () -> Command.parse "move shuai 1 to (1,2)"));

    "Test for index not integer" >::
    (fun _ -> assert_raises 
        (Command.Malformed "Name should be followed by an integer index. ") 
        (fun () -> Command.parse "move shuai-d to (1,2)"));

    "Test for invalid chess name" >::
    (fun _ -> assert_raises 
        (Command.Malformed "Invalid chess name. Valid chess names are Bing, Zu, Che, Ma, Pao, Shi, Xiang, Shuai, Jiang. ") 
        (fun () -> Command.parse "move shui-1 to (1,2)"));

    "Test for invalid chess name format, e.g. Shuai--1" >::
    (fun _ -> assert_raises 
        (Command.Malformed "Command should be given as <chessname-index> to (pos1, pos2).") 
        (fun () -> Command.parse "move shui--1 to (1,2)"));

    "Test for missing left parenthesis">::
    (fun _ -> assert_raises 
        (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                            (("Shuai", 1), (1, 2))))
        (fun () -> Command.parse "move shuai-1 to 1,2)"));

    "Test for missing right parenthesis">::
    (fun _ -> assert_raises 
        (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                            (("Shuai", 1), (1, 2))))
        (fun () -> Command.parse "move shuai-1 to (1,2"));

    "Test for missing both left and right parenthesis">::
    (fun _ -> assert_raises 
        (Command.DoYouMean ("Did you mean move Shuai-1 to (1, 2) ? [y/N]", 
                            (("Shuai", 1), (1, 2))))
        (fun () -> Command.parse "move shuai-1 to 1,2"));

    "Test for missing a comma">::
    (fun _ -> assert_raises 
        (Command.Malformed "Location should be written as (col, row). You might miss a comma. ")
        (fun () -> Command.parse "move shuai-1 to (1 2)"));

    "Test for pos1 not int">::
    (fun _ -> assert_raises 
        (Command.Malformed "Location should be written as (col, row). Your col or row might not be an integer. ") 
        (fun () -> Command.parse "move shuai-1 to (h,2)"));

    "Test for pos2 not int">::
    (fun _ -> assert_raises 
        (Command.Malformed "Location should be written as (col, row). Your col or row might not be an integer. ") 
        (fun () -> Command.parse "move shuai-1 to (1,1.2)"));

    "Test for pos1 out of bound">::
    (fun _ -> assert_raises 
        (Command.Malformed "Location (col, row), col should be integer between 1 and 9. ") 
        (fun () -> Command.parse "move che-2 to (10,10)"));

    "Test for pos2 out of bound">::
    (fun _ -> assert_raises 
        (Command.Malformed "Location (col, row), row should be integer between 1 and 10. ") 
        (fun () -> Command.parse "move che-2 to (9,11)"));

    "Test for something follow a quit">::
    (fun _ -> assert_raises (Command.DoYouQuit) 
        (fun () -> Command.parse "quit s"));

    "Test for nothing after keyword move">::
    (fun _-> assert_raises (Command.Malformed "Your command 'move' should be followed by <chess name>-<index> to (col, row). ")
        (fun () -> Command.parse "move "));

    "Test for empty command">::
    (fun _-> assert_raises (Command.Empty)
        (fun () -> Command.parse ""));
  ]

let make_remain_red_test 
    (name : string)
    (st : State.t)
    (expected_output : (Board.name * (int * int)) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.red_remain_chess st)
        ~printer:print_state)

let make_remain_black_test 
    (name : string)
    (st : State.t)
    (expeceted_output : (Board.name * (int * int)) list) : test = 
  name >:: (fun _-> 
      assert_equal expeceted_output (State.black_remain_chess st)
        ~printer:print_state)

let make_remain_red_test2
    (name : string)
    (st : State.t)
    (expected_output : (Board.name * (int * int)) list) : test = 
  name >:: (fun _ -> 
      assert_equal true (cmp_set_like_lists expected_output 
                           (State.red_remain_chess st)))

let make_remain_black_test2 
    (name : string)
    (st : State.t)
    (expected_output : (Board.name * (int * int)) list) : test = 
  name >:: (fun _-> 
      assert_equal true (cmp_set_like_lists expected_output 
                           (State.black_remain_chess st)))

let make_red_captured_test
    (name : string)
    (st : State.t)
    (expected_output : Board.name list) : test = 
  name >:: (fun _->
      assert_equal expected_output (State.current_red_captured st)
        ~printer:print_name)

let make_black_captured_test
    (name : string)
    (st : State.t)
    (expected_output : Board.name list) : test = 
  name >:: (fun _->
      assert_equal expected_output (State.current_black_captured st)
        ~printer:print_name)

let make_red_captured_test2
    (name : string)
    (st : State.t)
    (expected_output : Board.name list) : test = 
  name >:: (fun _->
      assert_equal true (cmp_set_like_lists expected_output 
                           (State.current_red_captured st)))

let make_black_captured_test2
    (name : string)
    (st : State.t)
    (expected_output : Board.name list) : test = 
  name >:: (fun _->
      assert_equal true (cmp_set_like_lists expected_output 
                           (State.current_black_captured st)))

(** Tests for bing*)
(** Test for moving bing upward in its initial state. *)
let bing_r7 = multi_move [(1, 6); (3, 6); (5, 6); (7, 6); (9, 6)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b init_state

(** Test for moving bing 1 step up, left or right when across the boundary. *)
let bing_r6 = multi_move [(1, 5); (3, 5); (5, 5); (7, 5); (9, 5)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b bing_r7

let bing_crossbond = multi_move [(1, 4); (2, 5); (6, 5); (8, 5); (9, 4)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b bing_r6

(** Test for bing in row 1. *)
let bing_r5 = multi_move [(1, 4); (3, 4); (5, 4); (7, 4); (9, 4)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b bing_r6

let bing_r4 = multi_move [(1, 3); (3, 3); (5, 3); (7, 3); (9, 3)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b bing_r5

let bing_r3 = multi_move [(1, 2); (3, 2); (5, 2); (7, 2); (9, 2)]
    [("Bing", 1); ("Bing", 2); ("Bing", 3); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red; Board.Red] b bing_r4

let bing_r2 = multi_move [(1, 1); (3, 1); (7, 1); (9, 1)]
    [("Bing", 1); ("Bing", 2); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red] b bing_r3

let bing_r1 = multi_move [(2, 1); (4, 1); (6, 1); (8, 1)]
    [("Bing", 1); ("Bing", 2); ("Bing", 4); ("Bing", 5)]
    [Board.Red; Board.Red; Board.Red; Board.Red] b bing_r2

(** Tests for zu *)
(** Test for zu moving 1 step down in initial pos. *)
let zu_r4 = multi_move [(1, 5); (3, 5); (5, 5); (7, 5); (9, 5)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b init_state

(** Test for moving zu 1 step up, left or right when across the boundary. *)
let zu_r5 = multi_move [(1, 6); (3, 6); (5, 6); (7, 6); (9, 6)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r4

let zu_crossbond = multi_move [(1, 7); (4, 6); (6, 6); (7, 7); (8, 6)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r5

(** Test for zu in row 10. *)
let zu_r6 = multi_move [(1, 7); (3, 7); (5, 7); (7, 7); (9, 7)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r5

let zu_r7 = multi_move [(1, 8); (3, 8); (5, 8); (7, 8); (9, 8)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r6

let zu_r8 = multi_move [(1, 9); (3, 9); (5, 9); (7, 9); (9, 9)]
    [("Zu", 1); ("Zu", 2); ("Zu", 3); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r7

let zu_r9 = multi_move [(1, 10); (3, 10); (7, 10); (9, 10)]
    [("Zu", 1); ("Zu", 2); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r8

let zu_r10 = multi_move [(2, 10); (4, 10); (6, 10); (8, 10)]
    [("Zu", 1); ("Zu", 2); ("Zu", 4); ("Zu", 5)]
    [Board.Black; Board.Black; Board.Black; Board.Black] 
    b zu_r9

(** Tests for black Shi. *)
let shi_b1 = multi_move [(5, 2)] [("Shi", 3)] [Board.Black] b init_state

let shi_b2 = multi_move [(5, 2)] [("Shi", 4)] [Board.Black] b init_state

let shi_bnext = multi_move [(4, 3)] [("Shi", 4)] [Board.Black] b shi_b2

let shi_bnext2 = multi_move [(5, 2)] [("Shi", 4)] [Board.Black] b shi_bnext

let shi_bnext3 = multi_move [(6, 3)] [("Shi", 3)] [Board.Black] b shi_b1

let shi_bnext4 = multi_move [(5, 2)] [("Shi", 3)] [Board.Black] b shi_bnext3

(** Tests for red Shi. *)
let shi_r1 = multi_move [(5, 9)] ["Shi", 1] [Board.Red] b init_state

let shi_r2 = multi_move [(5, 9)] ["Shi", 2] [Board.Red] b init_state

let shi_rnext = multi_move [(6, 8)] ["Shi", 1] [Board.Red] b shi_r1

let shi_rnext2 = multi_move [(5, 9)] ["Shi", 1] [Board.Red] b shi_rnext

(** Tests for Jiang *)
let jiang_move = multi_move [(6, 1)] [("Jiang", 1)] [Board.Black] b shi_b2

let jiang_move2 = multi_move [(4, 1)] [("Jiang", 1)] [Board.Black] b shi_b1

let jiang_move3 = multi_move [(5, 2)] [("Jiang", 1)] [Board.Black] b init_state

let jiang_next = multi_move [(5, 1); (4, 1)] [("Jiang", 1); ("Shi", 3)] 
    [Board.Black; Board.Black] b jiang_move2

let jiang_2next = multi_move [(4, 2)] [("Jiang", 1)] 
    [Board.Black] b jiang_move2

let jiang_2next2 = multi_move [(4, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next

let jiang_2next3 = multi_move [(5, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next2

let jiang_2next4 = multi_move [(4, 1); (5, 2)] [("Shi", 3); ("Jiang", 1)] 
    [Board.Black; Board.Black] b jiang_2next3

let jiang_2next4_2 = multi_move [(4, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next3

let jiang_2next4_3 = multi_move [(6, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next3

let jiang_2next5 = multi_move [(5, 1)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next4

let jiang_2next5_2 = multi_move [(5, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next4

let jiang_2next5_3 = multi_move [(4, 2)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next4

let jiang_2next5_4 = multi_move [(6, 2)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next4

let jiang_2next6 = multi_move [(6, 3)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next5_4

let jiang_next_3 = multi_move [(6, 2)] [("Jiang", 1)] 
    [Board.Black] b jiang_move

let jiang_2next7 = multi_move [(6, 2)] [("Jiang", 1)] 
    [Board.Black] b jiang_2next6

(** Tests for Shuai. *)
let shuai_move = multi_move [(5, 9)] ["Shuai", 1] [Board.Red] b init_state

let shuai_move_2 = multi_move [(4, 10)] ["Shuai", 1] [Board.Red] b shi_r1

let shuai_move_3 = multi_move [(6, 10)] ["Shuai", 1] [Board.Red] b shi_r2

let shuai_next = multi_move [(4, 9)] ["Shuai", 1] [Board.Red] b shuai_move_2

let shuai_next_2 = multi_move [(6, 9)] ["Shuai", 1] [Board.Red] b shuai_move_3

let shuai_next_3 = multi_move [(5, 8)] ["Shuai", 1] [Board.Red] b shuai_move

let shuai_next_3_2 = multi_move [(4, 9)] ["Shuai", 1] [Board.Red] b shuai_move

let shuai_next_3_3 = multi_move [(6, 9)] ["Shuai", 1] [Board.Red] b shuai_move

let shuai_next2 = multi_move [(4, 8)] ["Shuai", 1] [Board.Red] b shuai_next_3

let shuai_next2_2 = multi_move [(6, 8)] ["Shuai", 1] [Board.Red] b shuai_next_3

let shuai_next2_3 = multi_move [(4, 10)] ["Shuai", 1] [Board.Red] b shuai_next

let shuai_next2_4 = multi_move [(6, 8)] ["Shuai", 1] [Board.Red] b shuai_next_2

let shuai_next3 = multi_move [(4, 9)] ["Shuai", 1] [Board.Red] b shuai_next2

let shuai_next3_2 = multi_move [(5, 8)] ["Shuai", 1] [Board.Red] b shuai_next2

(** Tests for black Xiang. *)
let xiang_bmove = multi_move [(1, 3); (9, 3)] [("Xiang", 3); ("Xiang", 4)] 
    [Board.Black; Board.Black] b init_state

let xiang_bnext = multi_move [(3, 5); (7, 5)] [("Xiang", 3); ("Xiang", 4)] 
    [Board.Black; Board.Black] b xiang_bmove

let xiang_bnext2 = multi_move [(5, 3)] [("Xiang", 3)] [Board.Black] 
    b xiang_bnext

let xiang_bnext3 = multi_move [(3, 1)] [("Xiang", 3)] [Board.Black] 
    b xiang_bnext2

let xiangb_blocked = multi_move [2, 2] ["Pao", 3] [Board.Black] b init_state

let xiangb_occupied = multi_move [3, 5] ["Zu", 2] [Board.Black] b init_state

let xiangb_helper1 = multi_move [(6, 8)] [("Pao", 2)] [Board.Red] 
    b xiang_bnext

let xiangb_blocked2 = multi_move [6, 4] ["Pao", 2] [Board.Red] 
    b xiangb_helper1

let xiangb_helper2 = multi_move [(6, 8)] [("Pao", 2)] [Board.Red] 
    b xiang_bnext

let xiangb_blocked3 = multi_move [(6, 2); (5, 3)] [("Pao", 2); ("Xiang", 4)] 
    [Board.Red; Board.Black] b xiangb_helper1

let xiangb_bondhelper = multi_move [1, 6] ["Bing", 1] [Board.Red] b xiang_bnext2

(** Tests for red Xiang. *)
let xiangr_move = multi_move [(1, 8); (5, 8)] [("Xiang", 1); ("Xiang", 2)] 
    [Board.Red; Board.Red] b init_state

let xiangr_next = multi_move [(3, 6); (7, 6)] [("Xiang", 1); ("Xiang", 2)] 
    [Board.Red; Board.Red] b xiangr_move

let xiangr_next2 = multi_move [(5, 8); (9, 8)] [("Xiang", 1); ("Xiang", 2)] 
    [Board.Red; Board.Red] b xiangr_next

let xiangr_next3 = multi_move [(3, 10); (7, 10)] [("Xiang", 1); ("Xiang", 2)] 
    [Board.Red; Board.Red] b xiangr_next2

let xiangr_blocked = multi_move [2, 9] ["Pao", 1] [Board.Red] b init_state

let xiangr_blocked2 = multi_move [2, 7] ["Pao", 3] [Board.Black] b xiangr_next

let xiangr_blocked3 = multi_move [8, 7] ["Pao", 4] [Board.Black] b xiangr_next2

let xiangr_blocked4 = multi_move [8, 9] ["Pao", 2] [Board.Red] b xiangr_next3

let xiangr_bond = multi_move [1, 5] ["Zu", 1] [Board.Black] b xiangr_next2

let mared_2upper_right= 
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(8,6)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_upper_right=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(9,7)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_lower_right=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(9,9)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_2lower_right=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(8,10)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red] b ini_st

let mared_2upper_left=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(6,6)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_upper_left=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(5,7)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_lower_left=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(5,9)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

let mared_2lower_left=
  multi_move [(7,8);(7,6);(9,6);(5,6);(9,8);(5,9);(4,8);(6,10)] 
    [("Ma",2);("Bing",4);("Bing",5);("Bing",3);("Pao",2);("Shi",2);("Shi",2);
     ("Ma",2)] 
    [Board.Red; Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red] b ini_st

(*(in the center of board) front, back, left, right all blocked, 
  can't move to any location(8 test cases) *)
let mared_blocked=
  multi_move [(7,8);(5,6);(9,6);(5,9);(6,8);(9,9);(7,9)] 
    [("Ma",2);("Bing",3);("Bing",5);("Shi",1);("Shi",1);("Che",2);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red] 
    b ini_st

(*all eight positions originally can be moved to are occupied 
  (one black and one red for each pair) *)
let mared_upperright_occupied=
  multi_move [(7,8);(7,6);(8,10);(9,2);(6,2);(6,7);(8,7);(8,6);(1,2);(4,2);
              (4,9);(9,9);(8,6)] 
    [("Ma",2);("Bing",4);("Pao",2);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Che",4);
     ("Che",3);("Che",3);("Che",3);("Che",3);("Ma",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red] b ini_st

let mared_lowerright_occupied=
  multi_move [(7,8);(7,6);(8,10);(9,2);(6,2);(6,7);(8,7);(8,6);(1,2);(4,2);
              (4,9);(9,9);(9,9)] 
    [("Ma",2);("Bing",4);("Pao",2);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Che",4);
     ("Che",3);("Che",3);("Che",3);("Che",3);("Ma",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red] b ini_st

let mared_occupied_right=
  multi_move [(7,8);(7,6);(8,10);(9,2);(6,2);(6,7);(8,7);(8,6);(1,2);(4,2);
              (4,9);(9,9)] 
    [("Ma",2);("Bing",4);("Pao",2);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Che",4);
     ("Che",3);("Che",3);("Che",3);("Che",3)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black] 
    b ini_st

let mared_upperleft_occupied=
  multi_move [(7,8);(7,6);(9,2);(6,2);(6,6);(1,2);(4,2);(4,9);(5,9);(6,6)] 
    [("Ma",2);("Bing",4);("Che",4);("Che",4);("Che",4);("Che",3);("Che",3);
     ("Che",3);("Che",3);("Ma",2)]
    [Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Red]
    b ini_st

let mared_lowerleft_occupied=
  multi_move [(7,8);(7,6);(9,2);(6,2);(6,6);(1,2);(4,2);(4,9);(5,9);(5,9)] 
    [("Ma",2);("Bing",4);("Che",4);("Che",4);("Che",4);("Che",3);("Che",3);
     ("Che",3);("Che",3);("Ma",2)]
    [Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Red]
    b ini_st

let mared_occupied_left=
  multi_move [(7,8);(7,6);(9,2);(6,2);(6,6);(1,2);(4,2);(4,9);(5,9)] 
    [("Ma",2);("Bing",4);("Che",4);("Che",4);("Che",4);("Che",3);("Che",3);
     ("Che",3);("Che",3)]
    [Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black]
    b ini_st

(*(right top) off to the right *)
let mared_offboard =
  multi_move [(9,9)] [("Che",2)] [Board.Red] b ini_st

(*black ma *)
let mablack_2upper_right=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(4,5)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st 

let mablack_upper_right=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(5,4)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_lower_right=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(5,2)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_2lower_right=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(4,1)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_2upper_left=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(2,5)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_upper_left=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(1,4)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_lower_left=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(1,2)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_2lower_left=
  multi_move [(3,3);(1,5);(3,5);(5,5);(1,3);(5,2);(6,3);(2,1)] 
    [("Ma",3);("Zu",1);("Zu",2);("Zu",3);("Pao",3);("Shi",3);("Shi",3);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black]
    b ini_st

let mablack_blocked=
  multi_move [(3,3);(1,5);(5,5);(1,2);(3,2);(5,2);(4,3)]
    [("Ma",3);("Zu",1);("Zu",3);("Che",3);("Che",3);("Shi",3);("Shi",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black]
    b ini_st

let mablack_occupied=
  multi_move [(3,3);(2,1);(3,5);(1,3);(2,5);(1,9);(4,9);(4,2);(1,2);(8,5);(4,5);
              (9,9);(6,9);(6,2);(5,2)]
    [("Ma",3);("Che",3);("Zu",2);("Pao",3);("Pao",1);("Che",1);("Che",1);
     ("Che",1);("Che",1);("Pao",2);("Pao",2);("Che",2);("Che",2);("Che",2);
     ("Che",2)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red]
    b ini_st

let mablack_upperright_occupied=
  multi_move [(3,3);(2,1);(3,5);(1,3);(2,5);(1,9);(4,9);(4,2);(1,2);(8,5);(4,5);
              (9,9);(6,9);(6,2);(5,2);(4,5)]
    [("Ma",3);("Che",3);("Zu",2);("Pao",3);("Pao",1);("Che",1);("Che",1);
     ("Che",1);("Che",1);("Pao",2);("Pao",2);("Che",2);("Che",2);("Che",2);
     ("Che",2);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Black]
    b ini_st

let mablack_lowerright_occupied=
  multi_move [(3,3);(2,1);(3,5);(1,3);(2,5);(1,9);(4,9);(4,2);(1,2);(8,5);(4,5);
              (9,9);(6,9);(6,2);(5,2);(5,2)]
    [("Ma",3);("Che",3);("Zu",2);("Pao",3);("Pao",1);("Che",1);("Che",1);
     ("Che",1);("Che",1);("Pao",2);("Pao",2);("Che",2);("Che",2);("Che",2);
     ("Che",2);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Black]
    b ini_st

let mablack_upperleft_occupied=
  multi_move [(3,3);(2,1);(3,5);(1,3);(2,5);(1,9);(4,9);(4,2);(1,2);(8,5);(4,5);
              (9,9);(6,9);(6,2);(5,2);(2,5)]
    [("Ma",3);("Che",3);("Zu",2);("Pao",3);("Pao",1);("Che",1);("Che",1);
     ("Che",1);("Che",1);("Pao",2);("Pao",2);("Che",2);("Che",2);("Che",2);
     ("Che",2);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Black]
    b ini_st

let mablack_lowerleft_occupied=
  multi_move [(3,3);(2,1);(3,5);(1,3);(2,5);(1,9);(4,9);(4,2);(1,2);(8,5);(4,5);
              (9,9);(6,9);(6,2);(5,2);(1,2)]
    [("Ma",3);("Che",3);("Zu",2);("Pao",3);("Pao",1);("Che",1);("Che",1);
     ("Che",1);("Che",1);("Pao",2);("Pao",2);("Che",2);("Che",2);("Che",2);
     ("Che",2);("Ma",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Black]
    b ini_st

let mablack_offboard =
  multi_move [(1,2)] [("Che",3)] [Board.Black] b ini_st


(** red che*)
let chered_chessred=
  multi_move [(5,6);(5,5);(4,5);(4,4);(9,9);(4,9);(4,6);(1,9);(4,9);(5,9);
              (2,6);(8,6)]
    [("Bing",3);("Bing",3);("Bing",3);("Bing",3);("Che",2);("Che",2);
     ("Che",2);("Che",1);("Che",1);("Shi",1);("Pao",1);("Pao",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red]
    b ini_st

let chered_chessred_lessb=
  multi_move [(5,6);(5,5);(4,5);(4,4);(9,9);(4,9);(4,6);(1,9);(4,9);(5,9);
              (2,6);(8,6);(4,5)]
    [("Bing",3);("Bing",3);("Bing",3);("Bing",3);("Che",2);("Che",2);
     ("Che",2);("Che",1);("Che",1);("Shi",1);("Pao",1);("Pao",2);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red]
    b ini_st

let chered_chessred_moreb=
  multi_move [(5,6);(5,5);(4,5);(4,4);(9,9);(4,9);(4,6);(1,9);(4,9);(5,9);
              (2,6);(8,6);(4,8)]
    [("Bing",3);("Bing",3);("Bing",3);("Bing",3);("Che",2);("Che",2);
     ("Che",2);("Che",1);("Che",1);("Shi",1);("Pao",1);("Pao",2);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red]
    b ini_st

let chered_chessred_lessa=
  multi_move [(5,6);(5,5);(4,5);(4,4);(9,9);(4,9);(4,6);(1,9);(4,9);(5,9);
              (2,6);(8,6);(3,6)]
    [("Bing",3);("Bing",3);("Bing",3);("Bing",3);("Che",2);("Che",2);
     ("Che",2);("Che",1);("Che",1);("Shi",1);("Pao",1);("Pao",2);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red]
    b ini_st

let chered_chessred_morea=
  multi_move [(5,6);(5,5);(4,5);(4,4);(9,9);(4,9);(4,6);(1,9);(4,9);(5,9);
              (2,6);(8,6);(6,6)]
    [("Bing",3);("Bing",3);("Bing",3);("Bing",3);("Che",2);("Che",2);
     ("Che",2);("Che",1);("Che",1);("Shi",1);("Pao",1);("Pao",2);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red]
    b ini_st

let chered_chessblack_lessb=
  multi_move [(9,9);(4,9);(4,6);(2,6);(8,6);(9,2);(6,2);(6,9);(4,9);(1,2);
              (4,2);(4,4);(4,4)]
    [("Che",2);("Che",2);("Che",2);("Pao",3);("Pao",4);("Che",4);("Che",4);
     ("Che",4);("Che",4);("Che",3);("Che",3);("Che",3);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red]
    b ini_st

let chered_chessblack_moreb=
  multi_move [(9,9);(4,9);(4,6);(2,6);(8,6);(9,2);(6,2);(6,9);(4,9);(1,2);(4,2);
              (4,4);(4,9)]
    [("Che",2);("Che",2);("Che",2);("Pao",3);("Pao",4);("Che",4);("Che",4);
     ("Che",4);("Che",4);("Che",3);("Che",3);("Che",3);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red]
    b ini_st

let chered_chessblack_lessa=
  multi_move [(9,9);(4,9);(4,6);(2,6);(8,6);(9,2);(6,2);(6,9);(4,9);(1,2);(4,2);
              (4,4);(2,6)]
    [("Che",2);("Che",2);("Che",2);("Pao",3);("Pao",4);("Che",4);("Che",4);
     ("Che",4);("Che",4);("Che",3);("Che",3);("Che",3);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red]
    b ini_st

let chered_chessblack_morea=
  multi_move [(9,9);(4,9);(4,6);(2,6);(8,6);(9,2);(6,2);(6,9);(4,9);(1,2);(4,2);
              (4,4);(8,6)]
    [("Che",2);("Che",2);("Che",2);("Pao",3);("Pao",4);("Che",4);("Che",4);
     ("Che",4);("Che",4);("Che",3);("Che",3);("Che",3);("Che",2)]
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Red] b ini_st

(** black che*)
let cheblack_chessblack=
  multi_move [(9,2);(6,2);(6,5);(6,3);(8,5);(3,5);(5,5);(5,6);(6,6);(6,7)] 
    [("Che",4);("Che",4);("Che",4);("Pao",3);("Pao",4);("Zu",2);("Zu",3);
     ("Zu",3);("Zu",3);("Zu",3)] 
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let cheblack_chessblack_lessb=
  multi_move [(9,2);(6,2);(6,5);(6,3);(8,5);(3,5);(5,5);(5,6);(6,6);(6,7);(6,4)] 
    [("Che",4);("Che",4);("Che",4);("Pao",3);("Pao",4);("Zu",2);("Zu",3);
     ("Zu",3);("Zu",3);("Zu",3);("Che",4)] 
    [Board.Black;Board.Black;Board.Black;Board.Black; Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let cheblack_chessblack_moreb=
  multi_move [(9,2);(6,2);(6,5);(6,3);(8,5);(3,5);(5,5);(5,6);(6,6);(6,7);(6,6)] 
    [("Che",4);("Che",4);("Che",4);("Pao",3);("Pao",4);("Zu",2);("Zu",3);
     ("Zu",3);("Zu",3);("Zu",3);("Che",4)] 
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let cheblack_chessblack_lessa=
  multi_move [(9,2);(6,2);(6,5);(6,3);(8,5);(3,5);(5,5);(5,6);(6,6);(6,7);(5,5)] 
    [("Che",4);("Che",4);("Che",4);("Pao",3);("Pao",4);("Zu",2);("Zu",3);
     ("Zu",3);("Zu",3);("Zu",3);("Che",4)] 
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let cheblack_chessblack_morea=
  multi_move [(9,2);(6,2);(6,5);(6,3);(8,5);(3,5);(5,5);(5,6);(6,6);(6,7);(7,5)] 
    [("Che",4);("Che",4);("Che",4);("Pao",3);("Pao",4);("Zu",2);("Zu",3);
     ("Zu",3);("Zu",3);("Zu",3);("Che",4)] 
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let cheblack_chessred_lessb=
  multi_move [(2,5);(6,5);(6,3);(9,2);(4,2);(4,5);(6,5);(8,5);(9,8);(6,8);(1,9);
              (4,9);(4,5);(6,3)]
    [("Pao",1);("Pao",1);("Pao",1);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Pao",2);("Che",2);("Che",2);("Che",1);("Che",1);("Che",1);("Che",4)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Black] b ini_st

let cheblack_chessred_moreb=
  multi_move [(2,5);(6,5);(6,3);(9,2);(4,2);(4,5);(6,5);(8,5);(9,8);(6,8);(1,9);
              (4,9);(4,5);(6,8)]
    [("Pao",1);("Pao",1);("Pao",1);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Pao",2);("Che",2);("Che",2);("Che",1);("Che",1);("Che",1);("Che",4)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Black] b ini_st

let cheblack_chessred_lessa=
  multi_move [(2,5);(6,5);(6,3);(9,2);(4,2);(4,5);(6,5);(8,5);(9,8);(6,8);(1,9);
              (4,9);(4,5);(4,5)]
    [("Pao",1);("Pao",1);("Pao",1);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Pao",2);("Che",2);("Che",2);("Che",1);("Che",1);("Che",1);("Che",4)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Black] b ini_st

let cheblack_chessred_morea=
  multi_move [(2,5);(6,5);(6,3);(9,2);(4,2);(4,5);(6,5);(8,5);(9,8);(6,8);(1,9);
              (4,9);(4,5);(8,5)]
    [("Pao",1);("Pao",1);("Pao",1);("Che",4);("Che",4);("Che",4);("Che",4);
     ("Pao",2);("Che",2);("Che",2);("Che",1);("Che",1);("Che",1);("Che",4)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Black;Board.Black;
     Board.Black;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;Board.Red;
     Board.Black] b ini_st

(**red pao *)
let paored=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black] b ini_st  

let paored_lessb=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3);(6,4)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4);
     ("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Red] b ini_st                                                                                                           

let paored_moreb=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3);(6,7)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4);
     ("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Red] b ini_st

let paored_lessa=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3);(5,6)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4);
     ("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Red] b ini_st

let paored_morea=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3);(7,6)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4);
     ("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Red] b ini_st

let paored_flyblack=
  multi_move [(8,6);(6,6);(6,8);(8,6);(3,6);(9,3);(6,3);(6,1)] 
    [("Pao",2);("Pao",2);("Pao",1);("Pao",4);("Bing",2);("Che",4);("Che",4);
     ("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Red] b ini_st

let paored_edgecase=
  multi_move [(9,8);(8,9);(9,9);(9,10)][("Che",2);("Pao",2);("Pao",2);("Pao",2)] 
    [Board.Red;Board.Red;Board.Red;Board.Red] b ini_st

(**black pao *)
let paoblack =
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black] b ini_st

let paoblack_lessb=
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5);(6,4)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4);("Pao",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black;Board.Black] b ini_st

let paoblack_moreb=
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5);(6,7)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4);("Pao",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black;Board.Black] b ini_st

let paoblack_lessa=
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5);(5,5)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4);("Pao",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black;Board.Black] b ini_st

let paoblack_morea=
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5);(7,5)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4);("Pao",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black;Board.Black] b ini_st

let paoblack_flyred=
  multi_move [(8,5);(6,5);(6,3);(3,5);(6,8);(9,2);(8,2);(8,5);(6,10)] 
    [("Pao",4);("Pao",4);("Pao",3);("Zu",2);("Pao",2);("Che",4);("Che",4);
     ("Che",4);("Pao",4)]
    [Board.Black;Board.Black;Board.Black;Board.Black;Board.Red;Board.Black;
     Board.Black;Board.Black;Board.Black] b ini_st

let paoblack_edgecase=
  multi_move [(1,3);(2,2);(1,2);(1,1)] [("Che",3);("Pao",3);("Pao",3);("Pao",3)]
    [Board.Black;Board.Black;Board.Black;Board.Black] b ini_st

let state_tests =
  [
    make_remain_red_test2 "This is test for init red on board" ini_st 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];
    make_remain_black_test2 "Test is test for init black on board" ini_st
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_red_test2 "bingr7" bing_r7 
      [(("Bing", 5), (9, 6));(("Bing", 4),(7, 6));(("Bing", 3), (5, 6));
       (("Bing", 2), (3, 6));(("Bing", 1), (1, 6));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "red bing across bondary" bing_crossbond 
      [(("Bing", 5), (9, 4));(("Bing", 4),(8, 5));(("Bing", 3), (6, 5));
       (("Bing", 2), (2, 5));(("Bing", 1), (1, 4));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    "Test for bing move left in row 7" >:: 
    (fun _ -> assert_equal (State.Illegal)(State.move (2, 7) ("Bing", 2) 
                                             Board.Red b init_state));

    "Test for bing move down illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)(State.move (3, 6) ("Bing", 2) 
                                             Board.Red b bing_r6));

    "Test for bing in column 1 move left illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)(State.move (0, 5) ("Bing", 1) 
                                             Board.Red b bing_r6));

    "Test for bing in column 9 move right illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)(State.move (10, 5) ("Bing", 5) 
                                             Board.Red b bing_r6));

    make_remain_red_test2 "bing_r1" bing_r1 
      [(("Bing", 5), (8, 1));(("Bing", 4),(6, 1));(("Bing", 3), (5, 2));
       (("Bing", 2), (4, 1));(("Bing", 1), (2, 1));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_black_test2 "Test for eaten chess" bing_r1
      [(("Pao", 4),(8, 3));(("Pao", 3),(2, 3));(("Jiang", 1), (5, 1))];

    make_remain_black_test2 "zu_r4" zu_r4
      [(("Zu", 5),(9, 5));(("Zu", 4),(7, 5));(("Zu", 3),(5, 5));
       (("Zu", 2),(3, 5));(("Zu", 1),(1, 5));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_black_test2 " zu cross boundary" zu_crossbond
      [(("Zu", 5),(8, 6));(("Zu", 4),(7, 7));(("Zu", 3),(6, 6));
       (("Zu", 2),(4, 6));(("Zu", 1),(1, 7));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_black_test2 "zu_r10" zu_r10
      [(("Zu", 5),(8, 10));(("Zu", 4),(6, 10));(("Zu", 3),(5, 9));
       (("Zu", 2),(4, 10));(("Zu", 1),(2, 10));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    "Test for zu move right in row4" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (4, 4) ("Zu", 2) 
                                             Board.Black b init_state));

    "Test for zu move to its current pos illegal" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (3, 4) ("Zu", 2) 
                                             Board.Black b init_state));

    "Test for zu move up illegal" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (3, 3) ("Zu", 2) 
                                             Board.Black b init_state));

    "Test for zu in column 1 move left illegal" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (0, 10) ("Zu", 1) 
                                             Board.Black b zu_r9));

    "Test for zu in column 9 move right illegal" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (10, 10) ("Zu", 5) 
                                             Board.Black b zu_r9));

    make_remain_black_test2 "black shi1 init move" shi_b1 
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];  

    make_remain_black_test2 "black shi2 init move" shi_b2 
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(5, 2));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    make_remain_black_test2 "black shi2 move from 5,2" shi_bnext 
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(4, 3));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_black_test2 "black shi2 move from 4,3" shi_bnext2 
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(5, 2));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    make_remain_black_test2 "black shi1 move3" shi_bnext3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(6, 3));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    make_remain_black_test2 "black shi1 move4" shi_bnext4
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];       

    "Test for black shi cannot move to other position" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Shi", 3) 
                                             Board.Black b init_state));

    make_remain_red_test2 "red shi1 init move" shi_r1 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (5, 9));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "red shi2 init move" shi_r2 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(5, 9));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "red shi1 next move" shi_rnext 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (6, 8));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "red shi1 next2" shi_rnext2 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (5, 9));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    "Test for red shi cannot move to other position" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 10) ("Shi", 1) 
                                             Board.Red b shi_rnext));

    "Test for illegal movement when intended position is occcupied by chess of 
      same color" >:: 
    (fun _ -> assert_equal (State.Illegal)(State.move (6, 1) ("Jiang", 1) 
                                             Board.Black b init_state));                                        

    make_remain_black_test2 "Test for jiang init move" jiang_move
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(5, 2));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 1))];

    make_remain_black_test2 "Test for jiang init move2" jiang_move2
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (4, 1))];

    make_remain_black_test2 "Test for jiang init move3" jiang_move3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 2))];

    make_remain_black_test2 "jiang next move" jiang_next
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_black_test2 "Test for jiang 2nd next move" jiang_2next
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (4, 2))];

    make_remain_black_test2 "Test for jiang 2nd next move2" jiang_2next2
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (4, 3))];

    make_remain_black_test2 "Test for jiang 2nd next move3" jiang_2next3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 3))];

    make_remain_black_test2 "Test for jiang 2nd next move4" jiang_2next4
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 2))];

    make_remain_black_test2 "jiang 2nd next move4_2" jiang_2next4_2
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (4, 3))];

    make_remain_black_test2 "jiang 2nd next move4_3" jiang_2next4_3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(5, 2));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 3))];

    make_remain_black_test2 "Test for jiang move5" jiang_2next5
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];

    make_remain_black_test2 "Test for jiang move5_2" jiang_2next5_2
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 3))];

    make_remain_black_test2 "Test for jiang move5_3" jiang_2next5_3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (4, 2))];

    make_remain_black_test2 "Test for jiang move5_4" jiang_2next5_4
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 2))];

    make_remain_black_test2 "Test for jiang move6" jiang_2next6
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 3))];

    make_remain_black_test2 "Test for jiang move from 6,1" jiang_next_3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(5, 2));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 2))];

    make_remain_black_test2 "Test for jiang move7" jiang_2next7
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 1));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (6, 2))];

    "Test for jiang cannot move to other position from 5,1" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Jiang", 1) 
                                             Board.Black b init_state));

    "Test for jiang cannot move to other position from 4,1" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Jiang", 1) 
                                             Board.Black b jiang_move2));


    "Test for jiang cannot move to other position from 6,1" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Jiang", 1) 
                                             Board.Black b jiang_move));

    make_remain_red_test2 "shuai init move another pos" shuai_move_2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (5, 9));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 10))];

    make_remain_red_test2 "shuai init move possibility3" shuai_move_3
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(5, 9));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (6, 10))];

    make_remain_red_test2 "shuai next move from 4,10" shuai_next
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (5, 9));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 9))];

    make_remain_red_test2 "shuai next move from 6,10" shuai_next_2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(5, 9));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (6, 9))];

    make_remain_red_test2 "Test for shuai init move" shuai_move
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 9))];

    make_remain_red_test2 "shuai next move from 5,9" shuai_next_3
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 8))];

    make_remain_red_test2 "shuai next move from 5,9 -2" shuai_next_3_2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 9))];

    make_remain_red_test2 "shuai next move from 5,9 -3" shuai_next_3_3
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (6, 9))];

    make_remain_red_test2 "shuai next move from 5,8" shuai_next2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 8))];

    make_remain_red_test2 "shuai next move from 5,8 -2" shuai_next2_2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (6, 8))];

    make_remain_red_test2 "shuai next move from 4,9" shuai_next2_3
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (5, 9));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 10))];

    make_remain_red_test2 "shuai next move from 6,9" shuai_next2_4
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(5, 9));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (6, 8))];

    make_remain_red_test2 "shuai next move from 4,8" shuai_next3
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (4, 9))];

    make_remain_red_test2 "shuai next move from 6,8" shuai_next3_2
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 8))];

    "Test for shuai cannot move to other position from 5,10" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Shuai", 1) 
                                             Board.Red b init_state)); 

    "Test for shuai cannot move to other position from 6,8" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (7, 8) ("Shuai", 1) 
                                             Board.Red b shuai_next2_4));

    make_remain_black_test2 "Test for black xiang init move" xiang_bmove
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(9, 3));(("Xiang", 3),(1, 3));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    make_remain_black_test2 "Test for black xiang next move" xiang_bnext
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 5));(("Xiang", 3),(3, 5));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))];  

    make_remain_black_test2 "Test for black xiang next2" xiang_bnext2
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 5));(("Xiang", 3),(5, 3));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    make_remain_black_test2 "Test for black xiang next3" xiang_bnext3
      [(("Zu", 5),(9, 4));(("Zu", 4),(7, 4));(("Zu", 3),(5, 4));
       (("Zu", 2),(3, 4));(("Zu", 1),(1, 4));(("Shi", 4),(6, 1));
       (("Shi", 3),(4, 1));(("Xiang", 4),(7, 5));(("Xiang", 3),(3, 1));
       (("Ma", 4),(8, 1));(("Ma", 3),(2, 1));(("Pao", 4),(8, 3));
       (("Pao", 3),(2, 3));(("Che", 4),(9, 1));(("Che", 3),(1, 1)); 
       (("Jiang", 1), (5, 1))]; 

    "Black xiang cannot move if a black chess blocked path in the middle" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (1, 3) ("Xiang", 3) 
                                             Board.Black b xiangb_blocked)); 

    "Black xiang cannot move if a black chess blocked intended position" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (3, 5) ("Xiang", 3) 
                                             Board.Black b xiangb_occupied));

    "Black xiang cannot move if a red chess blocked path in the middle" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (5, 3) ("Xiang", 4) 
                                             Board.Black b xiangb_blocked2));

    "Black xiang cannot move if a chess blocked middle path, other direction" 
    >:: (fun _ -> assert_equal (State.Illegal)
            (State.move (7, 1) ("Xiang", 4) Board.Black b xiangb_blocked3));

    "Black xiang cannot move past the boundary" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (1, 7) ("Xiang", 3) 
                                             Board.Black b xiangb_bondhelper));

    make_remain_red_test2 "Test for red xiang init move" xiangr_move 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (5, 8));
       (("Xiang", 1), (1, 8));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "Test for red xiang next move" xiangr_next 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 6));
       (("Xiang", 1), (3, 6));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "Test for red xiang next2 move" xiangr_next2 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (9, 8));
       (("Xiang", 1), (5, 8));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    make_remain_red_test2 "Test for red xiang next3 move" xiangr_next3 
      [(("Bing", 5), (9, 7));(("Bing", 4),(7, 7));(("Bing", 3), (5, 7));
       (("Bing", 2), (3, 7));(("Bing", 1), (1, 7));(("Shi", 2),(6, 10));
       (("Shi", 1), (4, 10));(("Xiang", 2), (7, 10));
       (("Xiang", 1), (3, 10));(("Ma", 2), (8, 10));(("Ma", 1), (2, 10));
       (("Pao", 2), (8, 8));(("Pao", 1), (2, 8));(("Che", 2), (9, 10));
       (("Che", 1), (1, 10));(("Shuai", 1), (5, 10))];

    "Red xiang cannot move if a red chess blocked path in the middle" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (1, 8) ("Xiang", 1) 
                                             Board.Red b xiangr_blocked));

    "Red xiang cannot move if a black chess blocked middle path, another direc" 
    >:: (fun _ -> assert_equal (State.Illegal)(State.move (3, 6) ("Xiang", 1) 
                                                 Board.Red b xiangr_blocked2));

    "Red xiang2 cannot move if a black chess blocked middle path, another direc" 
    >:: (fun _ -> assert_equal (State.Illegal)(State.move (9, 8) ("Xiang", 2) 
                                                 Board.Red b xiangr_blocked3));

    "Red xiang2 cannot move if a red chess blocked middle path, another direc" 
    >:: (fun _ -> assert_equal (State.Illegal)(State.move (7, 10) ("Xiang", 2) 
                                                 Board.Red b xiangr_blocked4)); 

    "Red xiang cannot move past the boundary" >::
    (fun _ -> assert_equal (State.Illegal)(State.move (1, 4) ("Xiang", 1) 
                                             Board.Red b xiangr_bond));  
    make_remain_red_test2 ("ma red upper upper right unblocked")
      (mared_2upper_right) 
      [(("Bing",1),(1,7));(("Bing",2),(3,7)); (("Bing",3),(5,6));
       (("Bing",4),(7,6)); (("Bing",5),(9,6)); (("Pao",1),(2,8));
       (("Pao",2),(9,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(4,8));(("Xiang",2),(7,10));(("Ma",2),(8,6));
       (("Che",2),(9,10))];

    make_remain_red_test2 "ma red upper lower right unblocked"
      mared_upper_right 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(4,8));
       (("Xiang",2),(7,10));(("Ma",2),(9,7));(("Che",2),(9,10));
       (("Pao",2),(9,8))];

    make_remain_red_test2 "ma red lower upper right unblocked"
      mared_lower_right 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));
       (("Pao",2),(9,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(4,8));(("Xiang",2),(7,10));(("Ma",2),(9,9));
       (("Che",2),(9,10))];

    make_remain_red_test2 "ma red lower lower right unblocked"
      mared_2lower_right 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(4,8));
       (("Xiang",2),(7,10));(("Pao",2),(9,8));(("Che",2),(9,10));
       (("Ma",2),(8,10))];

    make_remain_red_test2 "ma red upper upper left unblocked"
      mared_2upper_left 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(4,8));
       (("Xiang",2),(7,10));(("Pao",2),(9,8));(("Che",2),(9,10));
       (("Ma",2),(6,6))];

    make_remain_red_test2 "ma red upper lower left unblocked"
      mared_upper_left 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));(("Ma",2),(5,7));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(4,8));
       (("Xiang",2),(7,10));(("Pao",2),(9,8));(("Che",2),(9,10))];

    make_remain_red_test2 "ma red lower upper left unblocked"
      mared_lower_left 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));(("Ma",2),(5,9));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(4,8));
       (("Xiang",2),(7,10));(("Pao",2),(9,8));(("Che",2),(9,10))];

    make_remain_red_test2 "ma red lower lower left unblocked"
      mared_2lower_left 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,6));
       (("Bing",4),(7,6));(("Bing",5),(9,6));(("Pao",1),(2,8));
       (("Pao",2),(9,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(4,8));(("Xiang",2),(7,10));(("Ma",2),(6,10));
       (("Che",2),(9,10))];

    "test for ma red upper upper right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (8,6) ("Ma",2) Board.Red b mared_blocked)); 

    "test for ma red upper lower right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (9,7) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red lower upper right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (9,9) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red lower lower right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (8,10) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red upper upper left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,6) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red upper lower left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (5,7) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red lower upper left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (5,9) ("Ma",2) Board.Red b mared_blocked));

    "test for ma red lower lower left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,10) ("Ma",2) Board.Red b mared_blocked));

    make_remain_red_test2 "ma red occupied legal upper right red check" 
      mared_upperright_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,6));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Pao",2),(8,10));(("Che",1),(1,10));(("Ma",1),(2,10));(("Ma",2),(8,6));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Che",2),(9,10))];

    make_remain_black_test2 "ma red occupied legal upper right black check"
      mared_upperright_occupied 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Che",3),(9,9));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));(("Ma",4),(8,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1))
      ];  

    make_remain_red_test2 "ma red occupied legal lower right red check"
      mared_lowerright_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,6));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Pao",2),(8,10));(("Che",1),(1,10));(("Ma",1),(2,10));(("Ma",2),(9,9));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Che",2),(9,10))];

    make_remain_black_test2 "ma red occupied legal lower right black check"
      mared_lowerright_occupied 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Ma",3),(2,1));
       (("Xiang",3),(3,1));(("Shi",3),(4,1));(("Jiang",1),(5,1));
       (("Shi",4),(6,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(8,6))];  

    "test for ma red occupied upper right illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (9,7) ("Ma",2) Board.Red b mared_occupied_right));

    "test for ma red occupied lower illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (8,10) ("Ma",2) Board.Red b mared_occupied_right));

    make_remain_red_test2 "ma_red_occupied_legal_upper_left"
      mared_upperleft_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,6));(("Bing",5),(9,7));(("Pao",1),(2,8));(("Ma",2),(6,6));
       (("Pao",2),(8,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Shi",1),(4,10));(("Xiang",1),(3,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Che",2),(9,10))];

    make_remain_black_test2 "ma_red_occupied_legal_upper_left"
      mared_upperleft_occupied 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Ma",3),(2,1));
       (("Che",3),(5,9));(("Xiang",3),(3,1));(("Shi",3),(4,1));(("Ma",4),(8,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1))];

    make_remain_red_test2 "ma_red_occupied_legal_lower_left"
      mared_lowerleft_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,6));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Pao",2),(8,8));(("Che",1),(1,10));(("Ma",1),(2,10));(("Ma",2),(5,9));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Che",2),(9,10))];

    make_remain_black_test2 "ma_red_occupied_legal_lower_left"
      mared_lowerleft_occupied 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Ma",3),(2,1));
       (("Xiang",3),(3,1));(("Shi",3),(4,1));(("Jiang",1),(5,1));
       (("Shi",4),(6,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(6,6))];

    "test for ma red occupied upper left illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (5,7) ("Ma",2) Board.Red b mared_occupied_left));

    "test for ma red occupied lower left illegal" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (6,10) ("Ma",2) Board.Red b mared_occupied_left));      


    "test for upper right ma red move off board to the top" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (7,12) ("Ma",2) Board.Red b ini_st)); 

    "test for upper right ma red move off board to the right" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (10,9) ("Ma",2) Board.Red b mared_offboard));                                                                      

    make_remain_black_test2 "ma_black_2upper_right" mablack_2upper_right 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(4,5));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_upper_right" mablack_upper_right 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(5,4));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_lower_right" mablack_lower_right 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(5,2));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_2lower_right" mablack_2lower_right 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(4,1));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_2upper_left" mablack_2upper_left 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(2,5));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_upper_left" mablack_upper_left 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(1,4));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_lower_right" mablack_lower_left 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(1,2));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "ma_black_2lower_left" mablack_2lower_left 
      [(("Zu",1),(1,5));(("Zu",2),(3,5));(("Zu",3),(5,5));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(6,3));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    "test for ma black upper upper right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (4,5) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black upper lower right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (5,4) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black lower upper right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (5,2) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black lower lower right blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (4,1) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black upper upper left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (2,5) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black upper lower left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (1,4) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black lower upper left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (1,2) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    "test for ma black lower lower left blocked" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (2,1) ("Ma",3) 
                                              Board.Black b mablack_blocked));

    make_remain_black_test2 "mablack_upperright_occupied" 
      mablack_upperright_occupied
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(2,1));
       (("Ma",3),(4,5));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "mablack_lowerright_occupied" 
      mablack_lowerright_occupied
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(2,1));
       (("Ma",3),(5,2));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "mablack_upperleft_occupied" 
      mablack_upperleft_occupied
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(2,1));
       (("Ma",3),(2,5));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_black_test2 "mablack_lowerleft_occupied" 
      mablack_lowerleft_occupied
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(1,3));(("Pao",4),(8,3));(("Che",3),(2,1));
       (("Ma",3),(1,2));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(9,1))];

    make_remain_red_test2 "mablack_upperright_occupied"
      mablack_upperright_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,5));
       (("Che",1),(1,2));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(5,2))];

    make_remain_red_test2 "mablack_lowerright_occupied"
      mablack_lowerright_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,5));
       (("Che",1),(1,2));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Pao",2),(4,5))];

    make_remain_red_test2 "mablack_upperleft_occupied"
      mablack_upperleft_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",2),(4,5));
       (("Che",1),(1,2));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(5,2))];

    make_remain_red_test2 "mablack_lowerleft_occupied"
      mablack_lowerleft_occupied 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,5));
       (("Ma",1),(2,10));(("Xiang",1),(3,10));(("Pao",2),(4,5));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(5,2))];

    "test for ma black upper right occupied" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (5,4) ("Ma",3) 
                                              Board.Black b mablack_occupied));

    "test for ma black lower right occupied" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (4,1) ("Ma",3) 
                                              Board.Black b mablack_occupied));

    "test for ma black upper left occupied" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (1,4) ("Ma",3) 
                                              Board.Black b mablack_occupied));

    "test for ma black lower left occupied" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (2,1) ("Ma",3) 
                                              Board.Black b mablack_occupied));

    "test for lower left ma black move off board to the bottom" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (1,-1) ("Ma",3) 
                                              Board.Black b mablack_offboard));

    "test for lower left ma black move off board to the left" >:: 
    (fun _ -> assert_equal (State.Illegal) (State.move (0,2) ("Ma",3) 
                                              Board.Black b mablack_offboard));

    make_remain_red_test2 "red_che_surround_by_red_chess_less_b"
      chered_chessred_lessb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(4,4));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,6));
       (("Ma",1),(2,10));(("Pao",2),(8,6));(("Che",1),(4,9));
       (("Xiang",1),(3,10));(("Shi",1),(5,9));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Ma",2),(8,10));
       (("Che",2),(4,5))];

    make_remain_red_test2 "red_che_surround_by_red_chess_more_b"
      chered_chessred_moreb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(4,4));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,6));
       (("Pao",2),(8,6));(("Che",1),(4,9));(("Ma",1),(2,10));(("Ma",2),(8,10));
       (("Xiang",1),(3,10));(("Shi",1),(5,9));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Che",2),(4,8))];

    make_remain_red_test2 "red_che_surround_by_red_chess_less_a"
      chered_chessred_lessa 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(4,4));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,6));
       (("Che",1),(4,9));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(5,9));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(3,6));
       (("Pao",2),(8,6))];

    make_remain_red_test2 "red_che_surround_by_red_chess_more_a"
      chered_chessred_morea 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(4,4));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,6));
       (("Che",1),(4,9));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(5,9));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(6,6));
       (("Pao",2),(8,6))];

    "test for che red less b onto red" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (4,4) ("Che",2) Board.Red b chered_chessred));

    "test for che red more b onto red" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (4,9) ("Che",2) Board.Red b chered_chessred));

    "test for che red less a onto red" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (2,6) ("Che",2) Board.Red b chered_chessred));

    "test for che red more a onto red" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (8,6) ("Che",2) Board.Red b chered_chessred)); 

    "test for che red less b after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (4,3) ("Che",2) Board.Red b chered_chessred));

    "test for che red more b after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (4,10)("Che",2) Board.Red b chered_chessred));

    "test for che red less a after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (1,6) ("Che",2) Board.Red b chered_chessred));

    "test for che red less b after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (9,6) ("Che",2) Board.Red b chered_chessred));                                                                                                                                         

    make_remain_red_test2 "red_che_surround_by_black_chess_less_b"
      chered_chessblack_lessb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(4,4));
       (("Pao",2),(8,8))];

    make_remain_red_test2 "red_che_surround_by_black_chess_more_b"
      chered_chessblack_moreb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(4,9));
       (("Pao",2),(8,8))];

    make_remain_red_test2 "red_che_surround_by_black_chess_less_a"
      chered_chessblack_lessa 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Pao",2),(8,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10));(("Xiang",2),(7,10));(("Ma",2),(8,10));
       (("Che",2),(2,6))];

    make_remain_red_test2 "red_che_surround_by_black_chess_more_a"
      chered_chessblack_morea 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Ma",2),(8,10));(("Bing",4),(7,7));(("Bing",5),(9,7));
       (("Pao",1),(2,8));(("Pao",2),(8,8));(("Che",1),(1,10));(("Ma",1),(2,10));
       (("Xiang",1),(3,10));(("Shi",1),(4,10));(("Shuai",1),(5,10));
       (("Shi",2),(6,10)); (("Xiang",2),(7,10));(("Che",2),(8,6))];

    make_remain_black_test2 "red_che_surround_by_black_chess_less_b"
      chered_chessblack_lessb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,6));(("Pao",4),(8,6));(("Ma",3),(2,1));
       (("Xiang",3),(3,1));(("Shi",3),(4,1));(("Jiang",1),(5,1));
       (("Shi",4),(6,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(4,9))];

    make_remain_black_test2 "red_che_surround_by_black_chess_more_b"
      chered_chessblack_moreb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,6));(("Pao",4),(8,6));(("Che",3),(4,4));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));(("Ma",4),(8,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1))];

    make_remain_black_test2 "red_che_surround_by_black_chess_less_a"
      chered_chessblack_lessa 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",4),(8,6));(("Che",3),(4,4));(("Ma",3),(2,1));
       (("Xiang",3),(3,1));(("Shi",3),(4,1));(("Jiang",1),(5,1));
       (("Shi",4),(6,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(4,9))];

    make_remain_black_test2 "red_che_surround_by_black_chess_more_a"
      chered_chessblack_morea 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,6));(("Che",3),(4,4));(("Ma",3),(2,1));
       (("Xiang",3),(3,1));(("Shi",3),(4,1));(("Jiang",1),(5,1));
       (("Shi",4),(6,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(4,9))];   

    "test for upper right che red move off board to the top" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (9,11) ("Che",2) Board.Red b ini_st));

    "test for upper right che red move off board to the right" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (10,10) ("Che",2) Board.Red b ini_st));

    make_remain_black_test2 "black_che_surround_by_black_chess_less_b"
      cheblack_chessblack_lessb 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(6,7));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(8,5));
       (("Che",3),(1,1));(("Ma",3),(2,1));(("Xiang",3),(3,1));
       (("Shi",3),(4,1));(("Jiang",1),(5,1));(("Shi",4),(6,1));
       (("Xiang",4),(7,1));(("Ma",4),(8,1));(("Che",4),(6,4))];

    make_remain_black_test2 "black_che_surround_by_black_chess_more_b"
      cheblack_chessblack_moreb 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(6,7));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(8,5));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));(("Ma",4),(8,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Che",4),(6,6))]; 

    make_remain_black_test2 "black_che_surround_by_black_chess_less_a"
      cheblack_chessblack_lessa 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(6,7));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(8,5));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(5,5))];

    make_remain_black_test2 "black_che_surround_by_black_chess_more_a"
      cheblack_chessblack_morea 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(6,7));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(8,5));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(7,5))];

    "test for che black less b onto black" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,3) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black more b onto black" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,7) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black less a onto black" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (3,5) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black more a onto black" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (8,5) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black less b after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,2) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black more b after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (6,8) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black less a after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (2,5) ("Che",4) 
           Board.Black b cheblack_chessblack));

    "test for che black more a after first chess" >:: 
    (fun _ -> assert_equal (State.Illegal) 
        (State.move (9,5) ("Che",4) 
           Board.Black b cheblack_chessblack));

    make_remain_black_test2 "black_che_surround_by_red_chess_less_b"
      cheblack_chessred_lessb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Ma",4),(8,1));
       (("Che",3),(1,1));(("Ma",3),(2,1));(("Xiang",3),(3,1));
       (("Shi",3),(4,1));(("Jiang",1),(5,1));(("Shi",4),(6,1));
       (("Xiang",4),(7,1));(("Che",4),(6,3))];

    make_remain_black_test2 "black_che_surround_by_red_chess_more_b"
      cheblack_chessred_moreb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(6,8))];

    make_remain_black_test2 "black_che_surround_by_red_chess_less_a"
      cheblack_chessred_lessa 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));
       (("Che",3),(1,1));(("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(4,5))];

    make_remain_black_test2 "black_che_surround_by_red_chess_more_a"
      cheblack_chessred_morea 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,3));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))];

    "test for lower left che black move off board to the bottom" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (1,0) ("Che",3) Board.Black b ini_st));

    "test for lower left che black move off board to the left" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (0,1)("Che",3) Board.Black b ini_st));

    make_remain_red_test2 "red_pao_state_less_b"
      paored_lessb 
      [(("Bing",1),(1,7));(("Bing",2),(3,6));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(6,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,4))];

    make_remain_red_test2 "red_pao_state_more_b"
      paored_moreb 
      [(("Bing",1),(1,7));(("Bing",2),(3,6));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(6,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,7))];

    make_remain_red_test2 "red_pao_state_less_a"
      paored_lessa 
      [(("Bing",1),(1,7));(("Bing",2),(3,6));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(6,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(5,6))];           

    make_remain_red_test2 "red_pao_state_more_a"
      paored_morea 
      [(("Bing",1),(1,7));(("Bing",2),(3,6));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(6,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(7,6))];    

    make_remain_black_test2 "red_pao_state_less_b"
      paored_lessb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,6));
       (("Che",3),(1,1));(("Ma",3),(2,1));(("Xiang",3),(3,1));
       (("Shi",3),(4,1));(("Jiang",1),(5,1));(("Shi",4),(6,1));
       (("Xiang",4),(7,1));(("Ma",4),(8,1));(("Che",4),(6,3))]; 

    make_remain_black_test2 "red_pao_state_more_b"
      paored_moreb 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,6));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(6,3))]; 

    make_remain_black_test2 "red_pao_state_less_a"
      paored_lessa 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,6));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(6,3))];    

    make_remain_black_test2 "red_pao_state_more_a"
      paored_morea 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,6));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(6,3))];  

    make_remain_red_test2 "red_pao_state_fly_black"
      paored_flyblack 
      [(("Bing",1),(1,7));(("Bing",2),(3,6));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(6,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,1))];    

    make_remain_black_test2 "red_pao_state_fly_black"
      paored_flyblack 
      [(("Zu",1),(1,4));(("Zu",2),(3,4));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(2,3));(("Pao",4),(8,6));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Xiang",4),(7,1));(("Ma",4),(8,1));
       (("Che",4),(6,3))];            

    "test for pao red fly to a red chess" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (6,10)("Pao",2) Board.Red b paored));

    "test for upper right pao red move off board to the top" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (9,11) ("Pao",2) Board.Red b paored_edgecase));

    "test for upper right pao red move off board to the right" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (10,10) ("Pao",2) Board.Red b paored_edgecase));  

    make_remain_black_test2 "black_pao_state_less_b"
      paoblack_lessb 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(6,4));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))];      

    make_remain_black_test2 "black_pao_state_more_b"
      paoblack_moreb 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(6,7));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))];

    make_remain_black_test2 "black_pao_state_less_a"
      paoblack_lessa 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(5,5));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))]; 

    make_remain_black_test2 "black_pao_state_more_a"
      paoblack_morea 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(7,5));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))];   

    make_remain_black_test2 "black_pao_state_fly_red"
      paoblack_flyred 
      [(("Zu",1),(1,4));(("Zu",2),(3,5));(("Zu",3),(5,4));(("Zu",4),(7,4));
       (("Zu",5),(9,4));(("Pao",3),(6,3));(("Pao",4),(6,10));(("Che",3),(1,1));
       (("Ma",3),(2,1));(("Xiang",3),(3,1));(("Shi",3),(4,1));
       (("Jiang",1),(5,1));(("Shi",4),(6,1));(("Xiang",4),(7,1));
       (("Ma",4),(8,1));(("Che",4),(8,5))];

    make_remain_red_test2 "black_pao_state_less_b"
      paoblack_lessb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,8))]; 

    make_remain_red_test2 "black_pao_state_more_b"
      paoblack_moreb 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,8))];

    make_remain_red_test2 "black_pao_state_less_a"
      paoblack_lessa 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,8))];

    make_remain_red_test2 "black_pao_state_more_a"
      paoblack_morea 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Shi",2),(6,10));
       (("Xiang",2),(7,10));(("Ma",2),(8,10));(("Che",2),(9,10));
       (("Pao",2),(6,8))];

    make_remain_red_test2 "black_pao_state_fly_red"
      paoblack_flyred 
      [(("Bing",1),(1,7));(("Bing",2),(3,7));(("Bing",3),(5,7));
       (("Bing",4),(7,7));(("Bing",5),(9,7));(("Pao",1),(2,8));
       (("Che",1),(1,10));(("Ma",1),(2,10));(("Xiang",1),(3,10));
       (("Shi",1),(4,10));(("Shuai",1),(5,10));(("Xiang",2),(7,10));
       (("Ma",2),(8,10));(("Che",2),(9,10));(("Pao",2),(6,8))];

    "test for pao red fly to a red chess" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (6,1) ("Pao",4) Board.Black b paoblack));

    "test for lower left pao black move off board to the bottom" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (1,0) ("Pao",3) Board.Black b paoblack_edgecase));

    "test for lower left pao black move off board to the left" >:: 
    (fun _ -> assert_equal (State.Illegal)
        (State.move (0,1) ("Pao",3) Board.Black b paoblack_edgecase));

  ]

let suite =
  "test suite for Final Project"  >::: List.flatten [
    board_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
