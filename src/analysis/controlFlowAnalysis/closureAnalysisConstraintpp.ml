open Printf

let token_to_string (token: Cubic.token): string =
  "&" ^ (Astpp.identifier_to_string token)

let rec tokens_to_string (tokens: Cubic.token list): string =
  match tokens with
  | [] -> ""
  | token :: [] -> (token_to_string token)
  | token :: tokens' -> (token_to_string token) ^ ", " ^ (tokens_to_string tokens')

let closure_constraint_to_string (incl_constraint: Cubic.incl_constraint): string =
  match incl_constraint with
  | Cubic.VarInclusion (var1, var2) -> "[[" ^ (Astpp.exp_to_string var1) ^ "]] subset [[" ^ (Astpp.exp_to_string var2) ^ "]]"
  | Cubic.TokenInclusion (tokens, var) -> "{" ^ (tokens_to_string tokens) ^ "} subset [[" ^ (Astpp.exp_to_string var) ^ "]]"
  | Cubic.ConditionalInclusion (token, var1, var2, var3) -> (token_to_string token) ^ " in [[" ^ (Astpp.exp_to_string var1) ^ "]] => [[" ^ (Astpp.exp_to_string var2) ^ "]] subset [[" ^(Astpp.exp_to_string var3) ^ "]]"

let pp_closure_constraint (incl_constraint: Cubic.incl_constraint) =
  printf "%s" (closure_constraint_to_string incl_constraint)

let pp_closure_constraints (instance: Cubic.instance) =
  printf "Tokens: %s" (tokens_to_string instance.Cubic.tokens); print_newline();
  printf "Variables: %s" (Astpp.exps_to_string instance.Cubic.variables); print_newline();
  print_endline "Cosntraints:";
  List.iter (fun (incl_constraint: Cubic.incl_constraint) ->
    printf "  "; pp_closure_constraint incl_constraint; print_newline()
  ) instance.Cubic.constraints