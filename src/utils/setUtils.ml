(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *
  * AST type produced by the parser.
  *)

module Const =
  struct
    type t = ConstantPropagationLattice.const
    let compare = compare
    let to_string = ConstantPropagationLattice.const_to_string
  end

module Sign =
  struct
    type t = SignLattice.sign
    let compare = compare
    let to_string = SignLattice.sign_to_string
  end

module String =
  struct
    type t = string
    let compare = compare
    let to_string = (fun str -> str)
  end

module ExpCmpId =
  struct
    type t = Ast.exp
    let compare e1 e2 =
      match e1.Ast.exp, e2.Ast.exp with
      | Ast.Identifier id1, Ast.Identifier id2 -> compare id1.Ast.identifier id2.Ast.identifier
      | _, _ -> compare e1.Ast.exp_id e2.Ast.exp_id
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
    let to_string = Astpp.simple_stm_to_string
  end

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
  end

module Make(Ord: OrderedType) = struct
  module OrdSet = Set.Make(Ord)
  
  let list_to_set xs =
    List.fold_left
      (fun acc x ->
        OrdSet.add x acc)
      OrdSet.empty xs
  
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