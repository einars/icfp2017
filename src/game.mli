open Types

(* probably will move non-server crap from punter.ml here *)

val is_mine : map_t -> int -> bool
val load_map : string -> map_t
val print_map : map_t -> unit
val new_game : map_t -> int -> game_t

val score : game_t -> player_t -> int

val find_site : site_t list -> int -> site_t
val find_mine : mine_t list -> int -> mine_t
val has_path_between_nodes : game_t -> player_t -> int -> int -> bool
