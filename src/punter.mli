
type player_t
type site_t
type river_t
type move_t
type map_t
type state_t

val load_map : string -> map_t
val print_map : map_t -> unit

val new_game : map_t -> int -> state_t

val host_game : state_t -> unit

