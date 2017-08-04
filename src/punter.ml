open Core
module P = Printf

type player_id_t = int

type player_t = {
  id: player_id_t;
  is_initialized: bool;
  name: string;
}

type site_t = {
  id: int;
  x: Yojson.Basic.json;
  y: Yojson.Basic.json;
}

type river_t = int * int
type mine_t = int

type move_t = {
  player_id: player_id_t;
  move: river_t;
}

type map_t = {
  source: string;
  sites: site_t list;
  rivers: river_t list;
  mines: mine_t list;
}


type state_t = {
  players: player_t list;
  moves: move_t list;
  map: map_t;
}

let json_whatever = Yojson.Basic.Util.member

let take_sites sjs =
  let open Yojson.Basic.Util in
  List.map sjs ~f:(fun elem ->
    let id = member "id" elem |> to_int in
    let x = json_whatever "x" elem in
    let y = json_whatever "y" elem in
    { id; x; y }
  )
;;

let take_rivers sjs =
  let open Yojson.Basic.Util in
  List.map sjs ~f:(fun elem ->
    let source = member "source" elem |> to_int in
    let target  = member "target" elem |> to_int in
    (source, target)
  )
;;

let take_mines sjs =
  List.map sjs ~f:Yojson.Basic.Util.to_int
;;


let load_map file_name = 
  P.printf "Loading map %s...\n" file_name;
  let mjs = Yojson.Basic.from_file file_name in
  let open Yojson.Basic.Util in
  {
    source = file_name;
    sites = mjs |> member "sites" |> to_list |> take_sites;
    rivers = mjs |> member "rivers" |> to_list |> take_rivers;
    mines = mjs |> member "mines" |> to_list |> take_mines;
  }
;;

let print_map m = 
  (* P.printf "%s [sites=%d, rivers=%d, mines=%d]\n" 
    (m.source)
    (List.length m.sites)
    (List.length m.rivers)
    (List.length m.mines);
    *)
  ()

;;


