(** Various utility modules and functions.

    To avoid prefixing functions with [Util] you can open the module
    in your source code: {[ open Util ]} *)

(**************************************************************)
(** {2 List Processing}                                      

These list processing functions provide general abstractions
for some commonly used patterns on lists.

{i Note to functional programmers: The arguments are in a non-standard
order make for a more readable inline usage.}

{i Please don't change the functions already implemented here.}
*)
(**************************************************************)

(** Iterate over a list of x's, [xs],
    executing a command, [f], on each x.

    For example:
{[# iter [1; 2; 3] (fun x ->
    print_int x;
    print_string " ")
  ;;
1 2 3 - : unit = () ]}
*)
let iter xs f = List.iter f xs

(** Iterate over the product of two lists: Xs * Ys.

    For example:
{[# iter_prod [1; 2] [3; 4] (fun x y ->
    print_int (x + y);
    print_string " ")
  ;;
4 5 5 6 - : unit = ()
]} *)
let iter_prod xs ys f =
  (* fold_prod xs ys () (fun x y () -> f x y) *)
  iter xs (fun x -> iter ys (fun y -> f x y))

(** Iterate over the product of a single list with itself: Xs * Xs,
    excluding pairs of the same entry (ie, the diagonal).

    For example:
{[# iter_pow2 [1; 2] (fun x y ->
    print_int (x + y);
    print_string " ")
  ;;
3 3 - : unit = ()
]} *)
let iter_pow2 xs f =
  iter_prod xs xs (fun x1 x2 -> if x1 == x2 then () else f x1 x2)

(** Fold over a list of x's, [xs],
    changing the previous result (or accumulator), [a],
    with the function, [f], for each x.

    For example:
{[# fold [1; 2; 3] 0 (fun x a ->
    x + a)
  ;;
-: int = 6 ]}
    Here [0] is the initial value of the accumulator [a].
    In (semi-valid) Java, this example could be translated as:
{[
int a = 0;
for (int x : List(1, 2, 3))
  a = x + a;
]}
*)
let fold xs a f = List.fold_left (fun a x -> f x a) a xs

(** Fold over the product of two lists: Xs * Ys.

    For example:
{[# fold_prod [1; 2] [3; 4] [] (fun x y zs ->
    (x,y) :: zs)
  ;;
-: (int * int) list = [(2, 4); (2, 3); (1, 4); (1, 3)]
]} *)
(* 'x list -> 'y list -> 'a -> ('x -> 'y -> 'a -> 'a) -> 'a *)
let fold_prod xs ys a f =
  fold xs a (fun x a -> fold ys a (fun y a -> f x y a))

(** Fold over the product of a single list with itself: Xs * Xs,
    excluding pairs of the same entry (ie, the diagonal).

    For example:
{[# fold_pow2 [1; 2] [] (fun x y zs ->
    (x,y) :: zs)
  ;;
-: (int * int) list = [(2, 1); (1, 2)]
]} *)
(* 'x list -> 'a -> ('a -> 'x -> 'x -> 'a) -> 'a *)
let fold_pow2 xs a f =
  fold_prod xs xs a (fun x1 x2 a -> if x1 == x2 then a else f x1 x2 a)
