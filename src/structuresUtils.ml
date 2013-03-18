module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module Make(Ord: OrderedType) = struct
  module OrdSet = Set.Make(Ord)
  
  let list_to_set xs =
    List.fold_left
      (fun acc x ->
        OrdSet.add x acc)
      OrdSet.empty xs
end