let make_big_F lambdas =
  List.fold_left
    (fun acc lambda -> (fun v -> acc (lambda v)))
    (fun i -> i) lambdas

let rec naive big_F x =
  let x' = big_F x in
  if x = x'
  then x
  else naive big_F x'