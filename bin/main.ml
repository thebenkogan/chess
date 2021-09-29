open Game
open Adventure
open Command
open State

(** [is_visited room lst] is true if [room] is in [lst] *)
let rec is_visited room lst =
  let filtered_list = List.filter (fun str -> str = room) lst in
  List.length filtered_list > 0

(** [print_description adv st visited] prints the description of the
    [st]'s current room in [adv] if [visited] is false. If [visited] is
    true, it prints a welcome back message. *)
let print_description adv st visited =
  if visited then
    print_endline
      ("\n Welcome back to " ^ State.current_room_id st ^ "! \n")
  else
    print_endline
      ("\n"
      ^ Adventure.description adv (State.current_room_id st)
      ^ " \n")

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [check_game_over adv st] checks if the adventurer has visited all of
    the rooms in [adv]. If they have, it prints a farewell message and
    ends the game. Otherwise, it is a [unit]. *)
let check_game_over adv st =
  match
    cmp_set_like_lists (Adventure.room_ids adv) (State.visited st)
  with
  | true ->
      print_endline
        "\n\
        \ Congratulations, you have explored the whole adventure! Well \
         done. \n";
      exit 0
  | false -> ()

(** [handle_state adv st visited] handles the current state of [adv]. If
    this is the adventurer's first time at [st]'s current room, it will
    print out a description of the room. Otherwise, it will welcome them
    back. If the adventurer has visited all of the rooms in [adv], it
    will print a farewell message and end the game. On a valid command
    from the adventurer, it will go to the next room if it exists,
    otherwise it will ask for them to try again. Invalid/empty commands
    will cause it to ask for them to try again. [visited] is true if the
    adventurer has already visited the current room of [st]. *)
let rec handle_state adv st visited =
  print_description adv st visited;
  check_game_over adv st;
  print_endline "Where would you like to go? \n";
  match read_line () with
  | input -> begin
      try
        match Command.parse input with
        | Go exit -> begin
            match State.go (String.concat " " exit) adv st with
            | Legal t ->
                handle_state adv t
                  (is_visited
                     (State.current_room_id t)
                     (State.visited st))
            | Illegal ->
                print_endline
                  "\n Exit does not exist. Please try again. \n";
                handle_state adv st visited
          end
        | Quit ->
            print_endline "\n Farewell adventurer! \n";
            exit 0
      with
      | Empty ->
          print_endline "\n Empty command. Please try again. \n";
          handle_state adv st visited
      | Malformed ->
          print_endline
            "\n Unrecognizable command. Please try again. \n";
          handle_state adv st visited
    end

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game f =
  try
    let adv = Adventure.from_json (Yojson.Basic.from_file f) in
    let state = State.init_state adv in
    handle_state adv state false
  with _ -> (
    print_endline "\n File not found. Please try again. \n";
    match read_line () with
    | exception End_of_file -> ()
    | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json"))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
