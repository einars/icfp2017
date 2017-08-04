open Core

module P = Printf

let _ = (

  let map = Punter.load_map Sys.argv.(1) in
  Punter.print_map map;

)
