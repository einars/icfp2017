open Types

(* probably will move non-server crap from punter.ml here *)

val is_mine : map_t -> int -> bool
val load_map : string -> map_t
val print_map : map_t -> unit
val new_game : map_t -> int -> game_t
