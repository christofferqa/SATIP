module CFG = ControlFlowGraph

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module Make(Ord: OrderedType) = struct
  module OrdMap = Map.Make(Ord)
  
  let pp_map map value_to_string =
    let rec map_to_string =
      (fun map ->
        if OrdMap.cardinal map >= 2 then
          let (key, value) = OrdMap.choose map in
          (Ord.to_string key) ^ " -> " ^ (value_to_string value) ^ ", " ^ (map_to_string (OrdMap.remove key map))
        else if OrdMap.cardinal map = 1 then
          let (key, value) = OrdMap.choose map in
          (Ord.to_string key) ^ " -> " ^ (value_to_string value) ^ " "
        else
          "") in
    Printf.printf "( %s)" (map_to_string map)
end