(**
  * Functions and type declarations having to do with error handling.
	*)

(** Function for signaling a "hole" that needes to completed towards a full tip compiler *)
let tip_not_implemented_yet file msg =
  Printf.printf "Implementation needed in %s:\n\t%s\n" file msg;
  exit 0 (* exit as a success to allow partial "negative" testing *)

(**/**) (*/*)

let print_position pos =
    let line = pos.Lexing.pos_lnum in
    let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Printf.printf " line %i, column %i:" line cnum 

let rec print_line pos =
  let inch = open_in pos.Lexing.pos_fname in
  let rec read_line i = match i with
    | 1 -> input_line inch;
    | n -> let _ = input_line inch in
	   read_line (i-1) in
  let line =
    try read_line pos.Lexing.pos_lnum
    with End_of_file ->
      print_endline "Warning: attempt to print invalid Lexing.position: line index out of bounds";
      "" in
  close_in inch;
  print_endline line;
  let rec print_col i col =
    if i > col
    then print_string "^"
    else begin
      (match line.[i] with
        | '\t' -> print_char '\t'
        | _    -> print_char ' ');
      print_col (i+1) col
    end
  in try print_col 0 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)
    with Invalid_argument _ ->
      print_newline ();
      print_endline "Warning: attempt to print invalid Lexing.position: column index out of bounds"

(**/**) (*/*)

(** Exit with error [errortype] at position [pos] and message [message] *)
let error pos message =
  if pos = Lexing.dummy_pos
  then
    begin
      print_string "Error: ";
      print_endline message;
			exit 0
    end
  else
    begin
      print_string ("File \"" ^ pos.Lexing.pos_fname ^"\",");
      print_position pos;
      print_newline ();
      print_string "Error: ";
      print_endline message;
      print_newline ();
      print_line pos;
      print_newline ();
      exit 0
    end