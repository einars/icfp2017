open Core
open Async
open Types


let keep_score = true

let log s = ksprintf (fun s -> printf "%s\n%!" (if (String.length s > 512) then String.sub ~pos:0 ~len:512 s else s)) s

module JU = Yojson.Basic.Util


let map_to_json ?ext:(ext=false) m =
  `Assoc [
    (* tk: "sites" should include the additional parameters as well *)
    "sites", `List (List.map m.sites ~f:(fun s -> 
      if ext then `Assoc [
        "id", `Int s.id;
        "x", s.x;
        "y", s.y;
      ] else `Assoc [
        "id", `Int s.id;
      ]));
    "rivers", `List (List.map m.rivers ~f:(fun r -> 
      if ext then `Assoc [
        "source", `Int r.source;
        "target", `Int r.target;
        "owner", (if r.owner = None then `Null else `Int (uw r.owner));
        ]
      else `Assoc [
        "source", `Int r.source;
        "target", `Int r.target;
      ] )
    );
    "mines", `List (List.map m.mines ~f:(fun m -> `Int m.id ));
  ]
;;





let choose_next_player game =
  List.find ~f:(fun e -> e.is_initialized = false) game.players


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
      }, 0)
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
  | Claim (id, river, score) -> `Assoc [ "claim", `Assoc [ "punter", `Int id; "source", `Int river.source; "target", `Int river.target; "score", `Int score]]
;;

let json_of_player_moves game =
  `List ( List.map game.players ~f:(fun p -> json_of_move p.last_move) )
;;

let json_of_all_moves game =
  `List ( List.map game.moves ~f:json_of_move )
;;

let json_of_game : game_t -> player_t -> json
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

let claim : game_t -> player_t -> river_t -> bool
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




let json_of_scores scores =
  `List (List.map scores ~f:(fun (id, name, score) -> `Assoc [
    "punter", `Int id;
    "name", `String name;
    "score", `Int score;
  ]))
;;
  
let scores game =
  List.map game.players ~f:(fun p -> p.id, p.name, Game.score game p + Futures.score game p)
;;

let host_game : game_t -> int -> unit Deferred.t =
  fun game port ->

  let iv_all_connected = Ivar.create() in


  let log_game () =
    let gamestate = `Assoc [
        "file", `String game.map.source;
        "scores", scores game |> json_of_scores;
        "moves", json_of_all_moves game;
        "map", map_to_json ~ext:true game.map
      ] in
    let now = Time.now () in
    let ts = Time.to_filename_string ~zone:(force Time.Zone.local) now  in
    let filename = sprintf "logs/%s.json" ts in
    Yojson.Basic.to_file filename gamestate;
    return ()
  in



  let send_scores () =

    let final_json = `Assoc [ "stop", `Assoc [
        "moves", json_of_player_moves game;
        "scores", scores game |> json_of_scores;
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
        | Claim (_, coords, _) ->
          if (claim game p coords) then (
            p.last_move <- Claim (p.id, coords, if keep_score then (Game.score game p + Futures.score game p) else -1);
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
      let setup = json_of_game game p |> Futures.augment_setup_message in
      send_and_read p setup 
      >>| fun setup ->
      Futures.augment_player game p setup
    ) |> await_all

    >>= play_loop
    >>= send_scores
    >>= log_game
    >>= fun _ ->
    log "That's all folks, shutting down.";
    server >>= Tcp.Server.close ~close_existing_connections:true
    >>= fun _ ->
    Ivar.fill_if_empty iv_shutdown ();
    return ()
  ));

  Ivar.read iv_shutdown

;;

