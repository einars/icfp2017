open Core
(*
Test strings:

19:{"me":"pipelstock"}


*)
open Async

type player_t = {
  id: int;
  mutable is_initialized: bool;
  mutable offline_state: string option;
  mutable name: string;
}

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
  | `Eof -> return err_empty_json
  | `Eof_without_delim whatever -> return err_empty_json
  | `Ok len_s ->
    printf "will read %s bytes\n%!" len_s;
    let len = int_of_string len_s in
    let buf = String.create len in
    Reader.read r buf ~len
    >>| (function
    | `Eof -> err_empty_json
    | `Ok len_s ->
      printf "Read [%s]" buf;
      Yojson.Basic.from_string buf
    )
  )
;;

let write_json_line w s =
  let data = sprintf "%d:%s" (String.length s) s in Writer.write w data
;;


let host_game game = (

  let all_connected_ivar = Ivar.create() in
  let all_connected = Ivar.read all_connected_ivar in

  let handshake _addr r w  = 

    match choose_next_player game with
    | None ->
      Writer.write w "All seats already taken, sorry";
      return ()

    | Some player -> (

      player.is_initialized <- true;
      printf "Got player %d\n%!" player.id;

      read_json_line r 
      >>= fun json ->
      let name = Yojson.Basic.Util.member "me" json |> Yojson.Basic.Util.to_string in
      printf "Got player name %s\n%!" name;
      player.name <- name;
      (sprintf {|{"you": "%s"}|} name) |> write_json_line w;
      if (choose_next_player game = None) then (
        printf "All players seem to be connected, let's try to notify the master\n%!";
        Ivar.fill all_connected_ivar true;
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

  upon (all_connected) (fun x -> 
    printf "Yes, I hear you.\n%!"
  );

)
;;

