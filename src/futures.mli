open Core
open Types

val augment_setup_message : json -> json

val augment_player : game_t -> player_t -> json -> unit

val score : game_t -> player_t -> int

