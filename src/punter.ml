open Core
(*
Test strings:

19:{"me":"pipelstock"}
11:{"ready":0}


*)
open Async

let log s = ksprintf (fun s -> printf "%s\n%!" s) s

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

type last_move_t = Pass | Claim of river_t

type player_t = {
  id: int;
  mutable is_initialized: bool;
  mutable offline_state: string option;
  mutable name: string;
  mutable last_move: last_move_t;
  mutable handle_r: Reader.t option;
  mutable handle_w: Writer.t option;
  mutable keepalive: unit Ivar.t option;
}


type move_t = {
  player_id: int;
  move: river_t;
}

type map_t = {
  source: string;
  sites: site_t list;
  rivers: river_t list;
  mines: int list;
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
    {
      source; target;
      owner = None
    }
  )
;;

let take_mines sjs =
  List.map sjs ~f:Yojson.Basic.Util.to_int
;;


let load_map file_name = 
  log "Loading map %s..." file_name;
  let mjs = Yojson.Basic.from_file file_name in
  let open Yojson.Basic.Util in
  {
    source = file_name;
    sites = mjs |> member "sites" |> to_list |> take_sites;
    rivers = mjs |> member "rivers" |> to_list |> take_rivers;
    mines = mjs |> member "mines" |> to_list |> take_mines;
  }
;;

let map_to_json m =
  `Assoc [
    (* tk: "sites" should include the additional parameters as well *)
    "sites", `List (List.map m.sites ~f:(fun s -> `Assoc [ "id", `Int s.id ]));
    "rivers", `List (List.map m.rivers ~f:(fun r -> `Assoc [ "source", `Int r.source; "target", `Int r.target ] ));
    "mines", `List (List.map m.mines ~f:(fun m -> `Assoc [ "id", `Int m ]));
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

let read_json_line r =
  (* Reader.read_until ~keep_delim:false r (`Pred (fun c -> not (Char.is_digit c)))  *)
  Reader.read_until ~keep_delim:false r (`Pred (fun c -> c = ':'))
  >>= function
  | `Eof -> log "eof"; return err_empty_json
  | `Eof_without_delim _ -> log "eof/no delim"; return err_empty_json
  | `Ok len_s ->
    let len = int_of_string (String.strip len_s) in
    let buf = String.create len in
    Reader.read r buf ~len
    >>| (function
    | `Eof -> log "Eof"; err_empty_json
    | `Ok _ ->
      log "Read [%s]" buf;
      Yojson.Basic.from_string buf
    )
;;

let read_from_player p = 
  log "read_from_player %d/%s" p.id p.name;
  read_json_line (uw p.handle_r)

let write_json_line w s =
  let data = sprintf "%d:%s\n" (String.length s) s in
  Writer.write w data;
  Writer.flushed w
;;


let send_and_read p some_string =
  log "send_and_read to %d/%s %s" p.id p.name some_string;
  write_json_line (uw p.handle_w) some_string 
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


let host_game : state_t -> int -> unit Deferred.t =
  fun game port ->

  let all_connected = Ivar.create() in
    

  let json_of_game : player_t -> json
    = fun p ->
    (`Assoc [
      "punter", `Int p.id;
      "punters", `Int (List.length game.players);
      "map", map_to_json game.map
    ])
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
      >>= fun json ->
      let name = Yojson.Basic.Util.member "me" json |> Yojson.Basic.Util.to_string in
      log "Player %d is now known as %s" player.id name;
      player.name <- name;
      (sprintf {|{"you": "%s"}|} name) |> write_json_line w 
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
      let setup = json_of_game p |> Yojson.Basic.to_string in
      send_and_read p setup
    ) in

    await_all setup_threads
    >>= fun _ ->
    server >>= Tcp.Server.close ~close_existing_connections:true
    >>= fun _ ->
    Ivar.fill iv_shutdown ();
    return ()

  ));

  Ivar.read iv_shutdown

;;

