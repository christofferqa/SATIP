module String =
  struct
    type t = string
    let compare = compare
    let to_string = (fun str -> str)
  end

module ExpCmpDesc =
  struct
    type t = Ast.exp
    let compare e1 e2 = compare (Astpp.exp_to_string e1) (Astpp.exp_to_string e2)
    let to_string = Astpp.exp_to_string
  end

module Stm =
  struct
    type t = Ast.stm
    let compare e1 e2 = compare e1.Ast.stm_id e2.Ast.stm_id
    let to_string = Astpp.stm_to_string
  end

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module Make(Ord: OrderedType) = struct
  module OrdSet = Set.Make(Ord)
  
  let pp_set set =
    let rec set_to_string =
      (fun set ->
        if (OrdSet.cardinal set) >= 2 then
          let value = OrdSet.choose set in
          (Ord.to_string value) ^ ", " ^ (set_to_string (OrdSet.remove value set))
        else if (OrdSet.cardinal set) = 1 then
          let value = OrdSet.choose set in
          (Ord.to_string value) ^ " "
        else
          "") in
    Printf.printf "{ %s}" (set_to_string set)
end