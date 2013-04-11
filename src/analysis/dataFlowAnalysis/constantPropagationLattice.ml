(**
  * @author Christoffer Quist Adamsen, cqa@cs.au.dk, christofferqa@gmail.com
  *)

type const =
  | Bottom
  | IntConst of int
  | QuestionMark

let const_to_string const =
  match const with
  | Bottom -> "bottom"
  | IntConst c -> string_of_int c
  | QuestionMark -> "?"

let least_upper_bound const1 const2 =
  if const1 = const2 then
    const1
  else
    match const1, const2 with
    | Bottom, _ -> const2
    | IntConst _, Bottom -> const1
    | IntConst _, IntConst _ -> QuestionMark
    | IntConst _, QuestionMark -> QuestionMark
    | QuestionMark, _ -> QuestionMark

let operator_abstract op const1 const2 =
  match const1, const2 with
  | Bottom, _ -> Bottom
  | _, Bottom -> Bottom
  | QuestionMark, _ -> QuestionMark
  | _, QuestionMark -> QuestionMark
  | IntConst c1, IntConst c2 -> 
    (match op with
    | Ast.Plus -> IntConst (c1 + c2)
    | Ast.Minus -> IntConst (c1 - c2)
    | Ast.Gt -> if c1 > c2 then IntConst 1 else IntConst 0
    | Ast.Times -> IntConst (c1 * c2)
    | Ast.Divide -> IntConst (c1 / c2)
    | Ast.Eq -> if c1 = c2 then IntConst 1 else IntConst 0)