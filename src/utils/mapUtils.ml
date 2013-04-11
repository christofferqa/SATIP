(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * AST type produced by the parser.
  *)

module CFG = ControlFlowGraph

module type Type =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module Make(T : Type) = struct
  module Map = Map.Make(T)
  
  let pp_map map value_to_string =
    let rec map_to_string =
      (fun map ->
        if Map.cardinal map >= 2 then
          let (key, value) = Map.choose map in
          (T.to_string key) ^ " -> " ^ (value_to_string value) ^ ", " ^ (map_to_string (Map.remove key map))
        else if Map.cardinal map = 1 then
          let (key, value) = Map.choose map in
          (T.to_string key) ^ " -> " ^ (value_to_string value) ^ " "
        else
          "") in
    Printf.printf "(%s)" (map_to_string map)
end