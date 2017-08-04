open Core

module P = Printf

let _ = (

  if (Array.length Sys.argv < 3) then
    printf "Usage: punter <n_players> <path/to/map.json>\n"
  else begin
    let n_players = int_of_string Sys.argv.(1) in
    let map = Punter.load_map Sys.argv.(2) in
    Punter.print_map map;

    let game = Punter.new_game map n_players in
    ()
  end


)
