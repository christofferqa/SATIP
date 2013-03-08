let fold xs a f = List.fold_left (fun a x -> f x a) a xs

let id = ref 0
let new_id = (fun old_id -> id := !id + 1; !id)