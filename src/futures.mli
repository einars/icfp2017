open Core
open Types

val augment_setup_message : json -> json

val augment_player : state_t -> player_t -> json -> unit

val score : state_t -> player_t -> int

