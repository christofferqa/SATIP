(**************************************************************)
(** {2 String Map}

This string map module specializes and extends the standard OCaml
map module for strings. For more info see the OCaml
{{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.html}
 Map module}.
*)
(**************************************************************)

include Map.Make (String)

let lookup k m = try Some (find k m) with Not_found -> None

(** This string map module includes all of the standard map functions
    in addition to a total (or explicitly partial) lookup function:
{[
    lookup: string -> 'a StringMap.t -> 'a option 
]}
    Usage:
{[
    match StringMap.lookup some_key some_map with
    | None   -> print_endline "No value for key" 
    | Some v -> print_endline "Found value for key"
]}
*)
