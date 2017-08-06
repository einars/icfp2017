open Core
open Types

module JU = Yojson.Basic.Util

let log s = ksprintf (fun s -> printf "%s\n%!" (if (String.length s > 512) then String.sub ~pos:0 ~len:512 s else s)) s

let json_get_map : json -> string -> json
= fun source k ->
  match source with 
  | `Assoc pairs -> (
    match List.Assoc.find ~equal:(String.equal) pairs k with
    | None -> `Assoc []
    | Some q -> q
  )
  | _ -> failwith "Json assoc expected"
;;

let json_get_list : json -> string -> 't list
= fun source k ->
  match source with 
  | `Assoc pairs -> (
    match List.Assoc.find ~equal:(String.equal) pairs k with
    | None -> []
    | Some q -> Yojson.Basic.Util.to_list q
  )
  | _ -> failwith "Json assoc expected"
;;


let json_set_map : json -> string -> json -> json
= fun source k v ->
  match source with 
  | `Assoc pairs ->
    `Assoc ((k, v) :: (List.Assoc.remove ~equal:(String.equal) pairs k))
  | _ -> failwith "Json assoc expected"


let augment_setup_message : json -> json
= fun setup_message ->
  let setup = json_get_map setup_message "setup" in
  let setup = json_set_map setup "futures" (`Bool true) in
  json_set_map setup_message "setup" setup
;;
    

let score _ _ = 0


let augment_player game player json =

  let used_futures = ref Int.Set.empty in

  let add_future_to_player fut =
    let source = JU.member "source" fut |> JU.to_int in
    let target = JU.member "target" fut |> JU.to_int in

    log "Player %d/%s creating future %d->%d" player.id player.name source target;

    if not (Game.is_mine game.map source) then (
      log "Future source is not a mine, skipping."
    ) else if Game.is_mine game.map target then (
      log "Future target is a mine, skipping."
    ) else if Set.mem !used_futures source then (
      log "Source already futured, skipping."
    ) else (
      used_futures := Set.add !used_futures source;
      let river = { source; target; owner = None } in
      player.futures <- river :: player.futures
    )

  in

  json_get_list json "futures" |> List.iter ~f:add_future_to_player;

  ()
;;
  

  
