open Core
(*
Test strings:

19:{"me":"pipelstock"}
11:{"ready":0}


*)
open Async


let log s = ksprintf (fun s -> printf "%s\n%!" s) s

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
  owner: int option
}

type move_t = Pass | Claim of river_t

type player_t = {
  id: int;
  mutable is_initialized: bool;
  mutable offline_state: string option;
  mutable name: string;
  mutable last_move: move_t;
  mutable handle_r: Reader.t option;
  mutable handle_w: Writer.t option;
  mutable keepalive: unit Ivar.t option;
}


type map_t = {
  source: string;
  sites: site_t list;
  rivers: river_t list;
  mines: int list;
}


type state_t = {
  players: player_t list;
  mutable moves: (int * move_t) list;
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
      last_move = Pass;
      handle_r = None;
      handle_w = None;
      keepalive = None;
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
  Reader.read_until ~keep_delim:false (uw player.handle_r) (`Pred (fun c -> c = ':'))
  >>= function
  | `Eof -> log "<< eof"; return err_empty_json
  | `Eof_without_delim _ -> log "<< eof/no delim"; return err_empty_json
  | `Ok len_s ->
    let len = int_of_string (String.strip len_s) in
    let buf = String.create len in
    Reader.read (uw player.handle_r) buf ~len
    >>| (function
    | `Eof -> log "<< Eof"; err_empty_json
    | `Ok _ ->
      log "<<%d %s" player.id buf;
      Yojson.Basic.from_string buf
    )
;;


let write_to_player : player_t -> json -> unit Deferred.t
= fun p data ->
  let s = Yojson.Basic.to_string data in
  let data = sprintf "%d:%s\n" (1 + String.length s) s in
  log ">>%d %s" p.id s;
  Writer.write (uw p.handle_w) data;
  Writer.flushed (uw p.handle_w)
;;


let decode_player_command : player_t -> json -> move_t
= fun p data ->
  let command = ref Pass in
  (match data with
  | `Assoc pts ->
    List.iter pts ~f:(fun (k,v) ->
      if (k = "claim")
      then command := Claim {
        source = JU.member "source" v |> JU.to_int;
        target = JU.member "target" v |> JU.to_int;
        owner = None;
      }
      else if (k = "pass") then command := Pass
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

let json_of_move player_id = function
  | Pass -> `Assoc [ "pass", `Assoc [ "punter", `Int player_id]]
  | Claim river -> `Assoc [ "claim", `Assoc [ "punter", `Int player_id; "source", `Int river.source; "target", `Int river.target ]]
;;

let json_of_player_moves game =
  `List ( List.map game.players ~f:(fun p -> json_of_move p.id p.last_move) )
;;

let json_of_game : state_t -> player_t -> json
  = fun game p ->
  (`Assoc [
    "punter", `Int p.id;
    "punters", `Int (List.length game.players);
    "map", map_to_json game.map
  ])
;;
  

let for_all_players game get_data process_response =
  let rec loop = function
    | [] -> return ()
    | p :: ps ->
      write_to_player p (get_data p)
      >>= fun _ -> read_from_player p
      >>= fun resp -> process_response p resp
      >>= fun _ -> loop ps
  in
  loop game.players
;;

let host_game : state_t -> int -> unit Deferred.t =
  fun game port ->

  let all_connected = Ivar.create() in

  let play_loop () =

    for_all_players game
      (fun p -> `Assoc [ "move", `Assoc [ "moves", json_of_player_moves game ]])
      (fun p resp ->
        let cmd = decode_player_command p resp  in
        match cmd with
        | Pass -> 
          p.last_move <- cmd;
          game.moves <- (p.id, cmd) :: game.moves;
          return ()
        | Claim coords ->
          p.last_move <- cmd;
          game.moves <- (p.id, cmd) :: game.moves;
          return ()
          (* TKTK: check, apply *)
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

      player.keepalive <- Some (Ivar.create ());

      log "Connected player %d" player.id;

      read_from_player player
      >>= fun data ->
      let name = JU.member "me" data |> JU.to_string in
      log "Player %d is now known as %s" player.id name;
      player.name <- name;
      `Assoc [ "you", `String name ] |> write_to_player player
      >>= fun _ ->
      if (choose_next_player game = None) then (
        log "All players seem to be connected, let's try to notify the master";
        Ivar.fill all_connected true;
      );
      (Ivar.read (uw player.keepalive))
    )
  in

  let server = Tcp.Server.create
    ~on_handler_error: `Raise
    (Tcp.on_port port)
    handshake
  in

  let iv_shutdown = Ivar.create () in

  upon (Ivar.read all_connected) (fun _ -> 
    ignore ( Writer.flushed (force Writer.stdout)
    >>= fun _ ->
    log "Yes, I hear you.";

    (* send setup to everybody *)

    let setup_threads = List.map game.players ~f:(fun p ->
      let setup = json_of_game game p in
      send_and_read p setup
    ) in

    await_all setup_threads
    >>= fun _ ->

    play_loop () 

    >>= fun _ ->
    server >>= Tcp.Server.close ~close_existing_connections:true
    >>= fun _ ->
    Ivar.fill iv_shutdown ();
    return ()

  ));

  Ivar.read iv_shutdown

;;

