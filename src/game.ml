open Core
open Types

let is_mine : map_t -> int -> bool
= fun map mine ->
  None <> List.find map.mines ~f:(fun m -> m = mine)
