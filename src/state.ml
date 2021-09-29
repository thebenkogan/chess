type t = {
  current_room : Adventure.room_id;
  visited_rooms : Adventure.room_id list;
}

let init_state adv =
  let starting_room = Adventure.start_room adv in
  { current_room = starting_room; visited_rooms = [ starting_room ] }

let current_room_id st = st.current_room

let visited st = List.sort_uniq compare st.visited_rooms

type result =
  | Legal of t
  | Illegal

let go ex adv st =
  try
    let next_room = Adventure.next_room adv st.current_room ex in
    Legal
      {
        current_room = next_room;
        visited_rooms = next_room :: st.visited_rooms;
      }
  with _ -> Illegal
