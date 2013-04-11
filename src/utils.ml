(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com.
  *)

let make_id =
  let count = ref 0 in
  (fun () ->
    let id = !count in
    count := id + 1;
    id)