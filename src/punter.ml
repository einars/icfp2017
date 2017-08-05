open Core
open Async


let log s = ksprintf (fun s -> printf "%s\n%!" (if (String.length s > 512) then String.sub ~pos:0 ~len:512 s else s)) s
let flush_stdout () = Writer.flushed (force Writer.stdout)

module JU = Yojson.Basic.Util
type json = Yojson.Basic.json

type site_t = {
  id: int;
  x: json;
  y: json;
}

type river_t = {
  source: int;
  target: int;
  mutable owner: int option
}

type move_t = Pass of int | Claim of int * river_t

type player_t = {
  id: int;
  mutable is_initialized: bool;
  mutable offline_state: string option;
  mutable name: string;
  mutable last_move: move_t;
  mutable handle_r: Reader.t option;
  mutable handle_w: Writer.t option;
  iv_keepalive: unit Ivar.t; (* ivar to signal that connection may be terminated *)
}


type map_t = {
  source: string;
  sites: site_t list;
  rivers: river_t list;
  mines: int list;
}


type state_t = {
  players: player_t list;
  mutable moves: move_t list;
  map: map_t;
}

let json_whatever = JU.member

let take_sites sjs =
  let open JU in
  List.map sjs ~f:(fun elem ->
    let id = member "id" elem |> to_int in
    let x = json_whatever "x" elem in
    let y = json_whatever "y" elem in
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

let map_to_json m =
  `Assoc [
    (* tk: "sites" should include the additional parameters as well *)
    "sites", `List (List.map m.sites ~f:(fun s -> `Assoc [ "id", `Int s.id ]));
    "rivers", `List (List.map m.rivers ~f:(fun r -> `Assoc [ "source", `Int r.source; "target", `Int r.target ] ));
    "mines", `List (List.map m.mines ~f:(fun m -> `Int m ));
  ]

let print_map m = 
  log "%s [sites=%d, rivers=%d, mines=%d]" 
    (m.source)
    (List.length m.sites)
    (List.length m.rivers)
    (List.length m.mines);
;;


let make_players n_players =
  let out = ref [] in
  for i = 0 to n_players - 1 do
    out := {
      id = i;
      is_initialized = false;
      name = "";
      offline_state = None;
      last_move = Pass i;
      handle_r = None;
      handle_w = None;
      iv_keepalive = Ivar.create ();
    } :: !out
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



let choose_next_player game =
  List.find ~f:(fun e -> e.is_initialized = false) game.players


let err_empty_json = Yojson.Basic.from_string "{}"


let read_from_player player =
  (* Reader.read_until ~keep_delim:false r (`Pred (fun c -> not (Char.is_digit c)))  *)
  Deferred.any [
    (after (sec 15.0) >>| fun _ -> Error "timeout");

    (Reader.read_until ~keep_delim:false (uw player.handle_r) (`Pred (fun c -> c = ':')) >>| function
      | `Eof -> Error "Eof"
      | `Eof_without_delim _ -> Error "eof/no delim"
      | `Ok len_s -> Ok len_s
    );
  ] >>= function
  | Error e -> log "%s" e; raise Exit
  | Ok len_s -> (
      let len = int_of_string (String.strip len_s) in
      let buf = String.create len in

      Deferred.any [
        (after (sec 15.0) >>| fun _ -> Error "timeout");
        (
        Reader.really_read (uw player.handle_r) buf ~len >>| (function
        | `Eof _ -> Error "eof"
        | `Ok ->
          log "<<%d %s" player.id (String.strip buf);
          Ok ( Yojson.Basic.from_string buf )
        ));
      ] >>| function
      | Ok res -> res
      | Error e -> log "%s" e; raise Exit
    )
;;


let write_to_player : player_t -> json -> unit Deferred.t
= fun p data ->
  let s = Yojson.Basic.to_string data in
  let data = sprintf "%d:%s\n" (1 + String.length s) s in
  log "%d>> %s" p.id s;
  Writer.write (uw p.handle_w) data;
  Writer.flushed (uw p.handle_w)
;;


let decode_player_command : player_t -> json -> move_t
= fun p data ->
  let command = ref (Pass p.id) in
  (match data with
  | `Assoc pts ->
    List.iter pts ~f:(fun (k,v) ->
      if (k = "claim")
      then command := Claim (p.id, {
        source = JU.member "source" v |> JU.to_int;
        target = JU.member "target" v |> JU.to_int;
        owner = None;
      })
      else if (k = "pass") then command := Pass p.id
    );
  | _ -> log "Some crap received, using pass"
  );
  !command
;;


let send_and_read : player_t -> json -> json Deferred.t
= fun p data ->
  write_to_player p data 
  >>= fun _ ->
  read_from_player p;
  (* probably parse futures here *)
;;


let await_all threads =
  let rec needle = function
  (* | h :: t -> h >>= needle t *)
  | h :: t -> 
    Deferred.bind h ~f:(fun _ -> needle t)
  | [] -> return ()
  in
  needle threads
;;

let json_of_move = function
  | Pass id -> `Assoc [ "pass", `Assoc [ "punter", `Int id]]
  | Claim (id, river) -> `Assoc [ "claim", `Assoc [ "punter", `Int id; "source", `Int river.source; "target", `Int river.target ]]
;;

let json_of_player_moves game =
  `List ( List.map game.players ~f:(fun p -> json_of_move p.last_move) )
;;

let json_of_game : state_t -> player_t -> json
  = fun game p ->
  (`Assoc [
    "punter", `Int p.id;
    "punters", `Int (List.length game.players);
    "map", map_to_json game.map
  ])
;;
  

let push_to_all_players game data =

  let rec loop = function
    | [] -> return ()
    | p :: ps ->
      write_to_player p data
      >>= fun _ -> loop ps
  in
  loop game.players
;;

let do_moves_for_all_players game get_data process_response =

  let n_moves = ref (List.length game.map.rivers) in

  let rec loop = function
    | [] -> loop game.players
    | p :: ps ->
      if (!n_moves = 0) then return ()
      else (
        n_moves := !n_moves - 1;
        write_to_player p (get_data p)
        >>= fun _ -> read_from_player p
        >>= fun resp -> process_response p resp
        >>= fun _ -> loop ps
      )
  in
  loop game.players
;;

let claim : state_t -> player_t -> river_t -> bool
= fun game player river ->
  let elem = List.find game.map.rivers ~f:(
    fun r -> (r.source = river.source && r.target = river.target) || (r.source = river.target && r.target = river.source)
  ) in
  (match elem with
  | None ->
    log "No such river";
    false
  | Some elem ->
    if elem.owner = None then (
      elem.owner <- Some player.id;
      true
    ) else (
      log "Already claimed by %d" (uw elem.owner);
      false
    )
  )
;;


let neighbour_nodes map node filter =
  List.map map.rivers ~f:(fun r ->
    if not (filter r) then None
    else if r.source = node then Some r.target
    else if r.target = node then Some r.source
    else None)
  |> List.filter ~f:(fun r -> r <> None)
  |> List.map ~f:uw
;;


let build_mine_score_map : map_t -> int -> int Int.Table.t
= fun map node_id ->

  let dists = Int.Table.create () in
  let queue = ref [] in

  let rec loop distance = function
  | h::t ->
    let nodes = neighbour_nodes map h (fun x -> true) in
    List.iter nodes ~f:(fun node_id ->
      if None <> Hashtbl.find dists node_id then ()
      else (
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


    
let calculate_mine_score game player mine =

  let score = ref 0 in
  let seen = ref [] in
  let queue = ref [] in

  let is_seen_node n = List.exists ~f:(fun x -> x = n) !seen
  in

  let dist_map = build_mine_score_map game.map mine in

  let nodes_belonging_to_player_out_of node =
    neighbour_nodes game.map node (fun r -> r.owner <> None && uw r.owner = player.id)
  in

  let rec loop = function
  | h::t ->
    let nodes = nodes_belonging_to_player_out_of h in
    List.iter nodes ~f:(fun node_id ->
      if not (is_seen_node node_id) then (
        seen := node_id :: !seen;
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

  loop [mine]
  
;;

let calculate_score game p =
  List.fold_left game.map.mines ~f:(fun score mine -> score + calculate_mine_score game p mine) ~init:0
;;

let calculate_scores game =
  List.map game.players ~f:(fun p -> p.id, p.name, calculate_score game p)
;;

let json_of_scores scores =
  `List (List.map scores ~f:(fun (id, name, score) -> `Assoc [
    "punter", `Int id;
    "name", `String name;
    "score", `Int score;
  ]))
;;
  

let host_game : state_t -> int -> unit Deferred.t =
  fun game port ->

  let iv_all_connected = Ivar.create() in

  let send_scores () =

    let final_json = `Assoc [ "stop", `Assoc [
        "moves", json_of_player_moves game;
        "scores", calculate_scores game |> json_of_scores;
      ]] in
    push_to_all_players game final_json

  in

  let play_loop () =

    do_moves_for_all_players game
      (fun _ -> `Assoc [ "move", `Assoc [ "moves", json_of_player_moves game ]])
      (fun p resp ->
        let cmd = decode_player_command p resp  in
        match cmd with
        | Pass _ -> 
          p.last_move <- cmd;
          game.moves <- cmd :: game.moves;
          return ()
        | Claim (_, coords) ->
          if (claim game p coords) then (
            p.last_move <- cmd;
          ) else (
            log "Claim failed";
            p.last_move <- Pass p.id;
          );
          game.moves <- p.last_move :: game.moves;
          return ()
      )
  in



    
  let handshake _addr r w  = 

    match choose_next_player game with
    | None ->
      Writer.write w "All seats already taken, sorry";
      return ()

    | Some player -> (

      player.is_initialized <- true;
      player.handle_r <- Some r;
      player.handle_w <- Some w;

      log "Connected player %d" player.id;

      read_from_player player
      >>= fun data ->
      let name = JU.member "me" data |> JU.to_string in
      log "Player %d is now known as %s" player.id name;
      player.name <- name;
      `Assoc [ "you", `String name ] |> write_to_player player
      >>= fun _ ->
      if (choose_next_player game = None) then Ivar.fill iv_all_connected true;
      Ivar.read player.iv_keepalive;
    )
  in

  let iv_shutdown = Ivar.create () in

  let server = Tcp.Server.create
    ~on_handler_error: `Raise
    (Tcp.on_port port)
    handshake
  in

  upon (Ivar.read iv_all_connected) (fun _ -> ignore (
    Writer.flushed (force Writer.stdout)
    >>= fun _ ->
    log "All players connected, starting game loop";
    List.map game.players ~f:(fun p ->
      let setup = json_of_game game p in
      send_and_read p setup
    ) |> await_all

    >>= play_loop
    >>= send_scores
    >>= fun _ ->
    log "That's all folks, shutting down.";
    server >>= Tcp.Server.close ~close_existing_connections:true
    >>= fun _ ->
    Ivar.fill_if_empty iv_shutdown ();
    return ()

  ));

  Ivar.read iv_shutdown

;;

