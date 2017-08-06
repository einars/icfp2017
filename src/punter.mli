open Async
open Types

val load_map : string -> map_t
val print_map : map_t -> unit

val new_game : map_t -> int -> state_t

val host_game : state_t -> int -> unit Deferred.t

