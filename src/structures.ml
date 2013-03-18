module CFG = ControlFlowGraph

module StringSet =
  Set.Make(String)

module IdentifierSet =
  Set.Make(String)

module ExpSetCmpDesc =
  Set.Make(struct
    type t = Ast.exp
    let compare e1 e2 = compare (Astpp.exp_to_string e1) (Astpp.exp_to_string e2)
  end)

let pp_exp_set_cmp_desc exp_set =
  let rec visit =
    (fun exp_set ->
      if (ExpSetCmpDesc.cardinal exp_set) >= 2 then
        let exp = ExpSetCmpDesc.choose exp_set in
        let () = Printf.printf "%s, " (Astpp.exp_to_string exp) in
        visit (ExpSetCmpDesc.remove exp exp_set)
      else if (ExpSetCmpDesc.cardinal exp_set) = 1 then
        let exp = ExpSetCmpDesc.choose exp_set in
        Printf.printf "%s" (Astpp.exp_to_string exp)) in
  Printf.printf "{";
  visit exp_set;
  Printf.printf "}"

module ExpSetCmpIdDesc =
  Set.Make(struct
    type t = Ast.exp
    let compare e1 e2 =
      match e1.Ast.exp, e2.Ast.exp with
      | Ast.Identifier id1, Ast.Identifier id2 -> compare id1.Ast.identifier id2.Ast.identifier
      | _, _ -> compare e1.Ast.exp_id e2.Ast.exp_id
  end)

let pp_exp_set_cmp_id_desc exp_set =
  let rec visit =
    (fun exp_set ->
      if (ExpSetCmpIdDesc.cardinal exp_set) >= 2 then
        let exp = ExpSetCmpIdDesc.choose exp_set in
        let () = Printf.printf "%s, " (Astpp.exp_to_string exp) in
        visit (ExpSetCmpIdDesc.remove exp exp_set)
      else if (ExpSetCmpIdDesc.cardinal exp_set) = 1 then
        let exp = ExpSetCmpIdDesc.choose exp_set in
        Printf.printf "%s" (Astpp.exp_to_string exp)) in
  Printf.printf "{";
  visit exp_set;
  Printf.printf "}"

module ExpSet =
  Set.Make(struct
    type t = Ast.exp
    let compare e1 e2 = compare e1.Ast.exp_id e2.Ast.exp_id
  end)

let pp_exp_set exp_set =
  let rec visit =
    (fun exp_set ->
      if (ExpSet.cardinal exp_set) >= 2 then
        let exp = ExpSet.choose exp_set in
        let () = Printf.printf "%s, " (Astpp.exp_to_string exp) in
        visit (ExpSet.remove exp exp_set)
      else if (ExpSet.cardinal exp_set) = 1 then
        let exp = ExpSet.choose exp_set in
        Printf.printf "%s" (Astpp.exp_to_string exp)) in
  Printf.printf "{";
  visit exp_set;
  Printf.printf "}"

module StmSet =
  Set.Make(struct
    type t = Ast.stm
    let compare e1 e2 = compare e1.Ast.stm_id e2.Ast.stm_id
  end)

let pp_stm_set stm_set =
  let rec visit =
    (fun stm_set ->
      if (StmSet.cardinal stm_set) >= 2 then
        let stm = StmSet.choose stm_set in
        let () = Printf.printf "%s, " (Astpp.stm_to_string stm) in
        visit (StmSet.remove stm stm_set)
      else if (StmSet.cardinal stm_set) = 1 then
        let stm = StmSet.choose stm_set in
        Printf.printf "%s" (Astpp.stm_to_string stm)) in
  Printf.printf "{";
  visit stm_set;
  Printf.printf "}"

module CFGNodeSet =
  Set.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)

module StringMap =
  Map.Make (String)

let pp_string_map str_map value_to_string =
  let rec visit =
    (fun str_map ->
      if (StringMap.cardinal str_map) >= 2 then
        let (str, value) = StringMap.choose str_map in
        let () = Printf.printf "%s -> %s, " str (value_to_string value) in
        visit (StringMap.remove str str_map)
      else if (StringMap.cardinal str_map) = 1 then
        let (str, value) = StringMap.choose str_map in
        Printf.printf "%s -> %s" str (value_to_string value)) in
  Printf.printf "(";
  visit str_map;
  Printf.printf ")"

module CFGNodeMap =
  Map.Make (struct
    type t = CFG.node
    let compare n1 n2 = compare (CFG.get_node_id n1) (CFG.get_node_id n2)
  end)