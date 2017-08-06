open Async

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

  mutable futures: river_t list;
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

