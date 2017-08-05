open Core
open Async

let _ = (

  if (Array.length Sys.argv < 3) then
    printf "Usage: punter <port> <n_players> <path/to/map.json>\n"
  else begin
    let port = int_of_string Sys.argv.(1) in
    let n_players = int_of_string Sys.argv.(2) in
    let map = Punter.load_map Sys.argv.(3) in
    Punter.print_map map;

    let game = Punter.new_game map n_players in

    ignore(
    Punter.host_game game port
    >>= fun _ -> Shutdown.exit 0
    );

    never_returns ( Scheduler.go () )

  end


)
