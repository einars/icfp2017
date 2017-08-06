open Core
open Async
open Types

module JU = Yojson.Basic.Util

let log s = ksprintf (fun s -> printf "%s\n%!" (if (String.length s > 512) then String.sub ~pos:0 ~len:512 s else s)) s

let is_mine : map_t -> int -> bool
= fun map mine ->
  None <> List.find map.mines ~f:(fun m -> m = mine)


let take_sites sjs =
  let open JU in
  List.map sjs ~f:(fun elem ->
    let id = member "id" elem |> to_int in
    let x = JU.member "x" elem in
    let y = JU.member "y" elem in
    { id; x; y }
  )
;;

let take_rivers sjs =
  let open JU in
  List.map sjs ~f:(fun elem ->
    let source = member "source" elem |> to_int in
    let target  = member "target" elem |> to_int in
    {
      source; target;
      owner = None
    }
  )
;;

let take_mines sjs =
  List.map sjs ~f:JU.to_int
;;


let load_map file_name = 
  log "Loading map %s..." file_name;
  let mjs = Yojson.Basic.from_file file_name in
  {
    source = file_name;
    sites = mjs |> JU.member "sites" |> JU.to_list |> take_sites;
    rivers = mjs |> JU.member "rivers" |> JU.to_list |> take_rivers;
    mines = mjs |> JU.member "mines" |> JU.to_list |> take_mines;
  }
;;

let blank_player i =
    {
      id = i;
      is_initialized = false;
      name = "";
      offline_state = None;
      last_move = Pass i;
      handle_r = None;
      handle_w = None;
      iv_keepalive = Ivar.create ();
      futures = [];
    }

let make_players n_players =
  let out = ref [] in
  for i = 0 to n_players - 1 do
    out := (blank_player i) :: !out
  done;
  List.rev !out
;;



let new_game map n_players = 
  {
    map;
    players = make_players n_players;
    moves = [];
  }
;;


let print_map m = 
  log "%s [sites=%d, rivers=%d, mines=%d]" 
    (m.source)
    (List.length m.sites)
    (List.length m.rivers)
    (List.length m.mines);
;;

