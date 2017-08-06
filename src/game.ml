open Core
open Async
open Types

module JU = Yojson.Basic.Util

let log s = ksprintf (fun s -> printf "%s\n%!" (if (String.length s > 512) then String.sub ~pos:0 ~len:512 s else s)) s

let is_mine : map_t -> int -> bool
= fun map mine ->
  None <> List.find map.mines ~f:(fun m -> m.id = mine)
;;


let find_site : site_t list -> int -> site_t
= fun sites id ->
  List.find sites ~f:(fun s -> s.id = id) |> uw
;;

let find_mine : mine_t list -> int -> mine_t
= fun mines id ->
  List.find mines ~f:(fun s -> s.id = id) |> uw
;;



let neighbour_nodes (rivers:river_t list) node =
  List.filter rivers ~f:(fun r -> r.source = node || r.target = node)
  |> List.map ~f:(fun r -> if r.source = node then (r.target, r) else (r.source, r))
;;



let take_sites ~rivers sjs =
  let open JU in
  List.map sjs ~f:(fun elem ->
    let id = member "id" elem |> to_int in
    let x = JU.member "x" elem in
    let y = JU.member "y" elem in
    { id; x; y; neighbors = neighbour_nodes rivers id }
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



let mine_distance_map ~sites node_id =

  let dists = Int.Table.create () in
  let queue = ref [] in

  let rec loop distance = function
  | h::t ->
    let node = find_site sites h in
    List.iter node.neighbors ~f:(fun (node_id, river) ->
      if None = Hashtbl.find dists node_id then (
        queue := node_id :: !queue;
        Hashtbl.set dists ~key:node_id ~data:distance
      );
    );
    loop distance t
  | [] -> 
    if !queue <> [] then (
      let nq = !queue in
      queue := [];
      loop (distance + 1) nq;
    )
  in

  Hashtbl.set dists ~key:node_id ~data:0;
  loop 1 [node_id];
  dists
;;




let take_mines ~sites sjs =
  List.map sjs ~f:(fun m -> 
    let id = JU.to_int m in
    {
      id;
      distances = mine_distance_map ~sites id
    }
  )
;;


let load_map source = 
  log "Loading map %s..." source;
  let mjs = Yojson.Basic.from_file source in
  let rivers = mjs |> JU.member "rivers" |> JU.to_list |> take_rivers in
  let sites = mjs |> JU.member "sites" |> JU.to_list |> take_sites ~rivers in
  {
    source;
    rivers;
    sites;
    mines = mjs |> JU.member "mines" |> JU.to_list |> take_mines ~sites;
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


let has_path_between_nodes : game_t -> player_t -> int -> int -> bool
= fun game player n1 n2 ->
  let seen = ref Int.Set.empty in
  let queue = ref [] in

  let is_seen_node n = Set.mem !seen n in

  let nodes_belonging_to_player_out_of node_id =
    let node = find_site game.map.sites node_id in
    List.filter node.neighbors ~f:( fun (node_id, river) -> river.owner <> None && uw river.owner = player.id)
    |> List.map ~f:fst
  in
  let rec loop = function
  | h::t ->
    nodes_belonging_to_player_out_of h
    |> List.iter ~f:(fun node_id ->
      if not (is_seen_node node_id) then (
        seen := Set.add !seen node_id;
        queue := node_id :: !queue;
      );
    );
    loop t
  | [] -> 
    if !queue <> [] then (
      let nq = !queue in
      queue := [];
      loop nq;
    )
  in

  loop [n1];
  Set.mem !seen n2
;;


    
let calculate_mine_score game player mine =

  let score = ref 0 in
  let seen = ref Int.Set.empty in
  let queue = ref [] in

  let is_seen_node n = Set.mem !seen n in

  let dist_map = mine.distances in

  let nodes_belonging_to_player_out_of node_id =
    let node = find_site game.map.sites node_id in
    List.filter node.neighbors ~f:( fun (node_id, node) -> node.owner <> None && uw node.owner = player.id)
    |> List.map ~f:fst
  in

  let rec loop = function
  | h::t ->
    let nodes = nodes_belonging_to_player_out_of h in
    List.iter nodes ~f:(fun node_id ->
      if not (is_seen_node node_id) then (
        seen := Set.add !seen node_id;
        queue := node_id :: !queue;
        let distance = Hashtbl.find_exn dist_map node_id in
        score := !score + distance * distance
      );
    );
    loop t
  | [] -> 
    if !queue = [] then !score
    else (
      let nq = !queue in
      queue := [];
      loop nq;
    )
  in

  loop [mine.id]
  
;;

let score game p =
  List.fold_left game.map.mines ~f:(fun score mine -> score + calculate_mine_score game p mine) ~init:0
;;

