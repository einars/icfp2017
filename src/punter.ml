open Core
(*
Test strings:

19:{"me":"pipelstock"}
11:{"ready":0}


*)
open Async

type site_t = {
  id: int;
  x: Yojson.Basic.json;
  y: Yojson.Basic.json;
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
  printf "Loading map %s...\n" file_name;
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
  printf "%s [sites=%d, rivers=%d, mines=%d]\n" 
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
  >>= (function
  | `Eof -> printf "eof\n%!"; return err_empty_json
  | `Eof_without_delim s -> printf "eof/no delim\n%!"; return err_empty_json
  | `Ok len_s ->
    printf "will read %s bytes\n%!" len_s;
    let len = int_of_string (String.strip len_s) in
    let buf = String.create len in
    Reader.read r buf ~len
    >>| (function
    | `Eof -> err_empty_json
    | `Ok _ ->
      printf "Read [%s]\n%!" buf;
      Yojson.Basic.from_string buf
    )
  )
;;

let write_json_line w s =
  let data = sprintf "%d:%s" (String.length s) s in Writer.write w data
;;


let send_and_read p some_string =
  write_json_line (uw p.handle_w) some_string;
  read_json_line (uw p.handle_r)





let host_game game = (

  let all_connected = Ivar.create() in
    

  let game_to_json_for_player p =
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

      printf "Got player %d\n%!" player.id;

      read_json_line r 
      >>= fun json ->
      let name = Yojson.Basic.Util.member "me" json |> Yojson.Basic.Util.to_string in
      printf "Got player name %s\n%!" name;
      player.name <- name;
      (sprintf {|{"you": "%s"}|} name) |> write_json_line w;
      if (choose_next_player game = None) then (
        printf "All players seem to be connected, let's try to notify the master\n%!";
        Ivar.fill all_connected true;
      );
      return ()
    )
  in



  let server = Tcp.Server.create
    ~on_handler_error: `Raise
    (Tcp.on_port 5000)
    handshake
  in
  ignore ( server );

  upon (Ivar.read all_connected) (fun _ -> 
    printf "Yes, I hear you.\n%!";

    (* send setup to everybody *)

    let setup_threads = List.map game.players ~f:(fun p ->
      let setup = game_to_json_for_player p |> Yojson.Basic.to_string in
      send_and_read p setup
    ) in
    ()

    (* await_all setup_threads; *)
    
    (* assume setup is done *)

  );

)
;;

