type sign =
| QuestionMark
| PlusZero
| MinusZero
| Plus
| Zero
| Minus
| One
| Bottom

let sign_to_string sign =
  match sign with
  | QuestionMark -> "?"
  | PlusZero -> "+0"
  | MinusZero -> "-0"
  | Plus -> "+"
  | Zero -> "0"
  | Minus -> "-"
  | One -> "1"
  | Bottom -> "bottom"

let rec least_upper_bound sign1 sign2 =
  if sign1 = sign2 then
    sign1
  else
    match sign1, sign2 with
    | Bottom, _ -> sign2
    | One, Plus -> Plus
    | One, Zero -> PlusZero
    | One, Minus -> QuestionMark
    | One, PlusZero -> PlusZero
    | One, MinusZero -> QuestionMark
    | Plus, Zero -> PlusZero
    | Plus, Minus -> QuestionMark
    | Plus, PlusZero -> PlusZero
    | Plus, MinusZero -> QuestionMark
    | Zero, Minus -> MinusZero
    | Zero, PlusZero -> PlusZero
    | Zero, MinusZero -> MinusZero
    | Minus, PlusZero -> QuestionMark
    | Minus, MinusZero -> MinusZero
    | PlusZero, MinusZero -> QuestionMark
    | QuestionMark, _ -> QuestionMark
    | _, _ -> least_upper_bound sign2 sign1

let operator_abstract op sign1 sign2 =
  match op with
  | Ast.Plus ->
    (match sign1, sign2 with
    | Bottom, _ -> Bottom
    | One, Bottom -> Bottom
    | One, One -> Plus
    | One, Plus -> Plus
    | One, Zero -> One
    | One, Minus -> MinusZero
    | One, PlusZero -> Plus
    | One, MinusZero -> QuestionMark
    | One, QuestionMark -> QuestionMark
    | Plus, Bottom -> Bottom
    | Plus, One -> Plus
    | Plus, Plus -> Plus
    | Plus, Zero -> Plus
    | Plus, Minus -> QuestionMark
    | Plus, PlusZero -> Plus
    | Plus, MinusZero -> QuestionMark
    | Plus, QuestionMark -> QuestionMark
    | Zero, Bottom -> Bottom
    | Zero, One -> One
    | Zero, Plus -> Plus
    | Zero, Zero -> Zero
    | Zero, Minus -> Minus
    | Zero, PlusZero -> PlusZero
    | Zero, MinusZero -> MinusZero
    | Zero, QuestionMark -> QuestionMark
    | Minus, Bottom -> Bottom
    | Minus, One -> MinusZero
    | Minus, Plus -> QuestionMark
    | Minus, Zero -> Minus
    | Minus, Minus -> Minus
    | Minus, PlusZero -> QuestionMark
    | Minus, MinusZero -> Minus
    | Minus, QuestionMark -> QuestionMark
    | PlusZero, Bottom -> Bottom
    | PlusZero, One -> Plus
    | PlusZero, Plus -> Plus
    | PlusZero, Zero -> PlusZero
    | PlusZero, Minus -> QuestionMark
    | PlusZero, PlusZero -> PlusZero
    | PlusZero, MinusZero -> QuestionMark
    | PlusZero, QuestionMark -> QuestionMark
    | MinusZero, Bottom -> Bottom
    | MinusZero, One -> QuestionMark
    | MinusZero, Plus -> QuestionMark
    | MinusZero, Zero -> MinusZero
    | MinusZero, Minus -> Minus
    | MinusZero, PlusZero -> QuestionMark
    | MinusZero, MinusZero -> MinusZero
    | MinusZero, QuestionMark -> QuestionMark
    | QuestionMark, Bottom -> Bottom
    | QuestionMark, _ -> QuestionMark)
  | Ast.Minus ->
    (match sign1, sign2 with
    | Bottom, _ -> Bottom
    | One, Bottom -> Bottom
    | One, One -> Zero
    | One, Plus -> MinusZero
    | One, Zero -> One
    | One, Minus -> Plus
    | One, PlusZero -> QuestionMark
    | One, MinusZero -> Plus
    | One, QuestionMark -> QuestionMark
    | Plus, Bottom -> Bottom
    | Plus, One -> PlusZero
    | Plus, Plus -> QuestionMark
    | Plus, Zero -> Plus
    | Plus, Minus -> Plus
    | Plus, PlusZero -> QuestionMark
    | Plus, MinusZero -> Plus
    | Plus, QuestionMark -> QuestionMark
    | Zero, Bottom -> Bottom
    | Zero, One -> MinusZero
    | Zero, Plus -> Minus
    | Zero, Zero -> Zero
    | Zero, Minus -> Plus
    | Zero, PlusZero -> MinusZero
    | Zero, MinusZero -> PlusZero
    | Zero, QuestionMark -> QuestionMark
    | Minus, Bottom -> Bottom
    | Minus, One -> Minus
    | Minus, Plus -> Minus
    | Minus, Zero -> Minus
    | Minus, Minus -> Minus
    | Minus, PlusZero -> Minus
    | Minus, MinusZero -> QuestionMark
    | Minus, QuestionMark -> QuestionMark
    | PlusZero, Bottom -> Bottom
    | PlusZero, One -> QuestionMark
    | PlusZero, Plus -> QuestionMark
    | PlusZero, Zero -> PlusZero
    | PlusZero, Minus -> Plus
    | PlusZero, PlusZero -> QuestionMark
    | PlusZero, MinusZero -> PlusZero
    | PlusZero, QuestionMark -> QuestionMark
    | MinusZero, Bottom -> Bottom
    | MinusZero, One -> Minus
    | MinusZero, Plus -> Minus
    | MinusZero, Zero -> MinusZero
    | MinusZero, Minus -> QuestionMark
    | MinusZero, PlusZero -> MinusZero
    | MinusZero, MinusZero -> QuestionMark
    | MinusZero, QuestionMark -> QuestionMark
    | QuestionMark, Bottom -> Bottom
    | QuestionMark, _ -> QuestionMark)
  | Ast.Gt ->
    (match sign1, sign2 with
    | Bottom, _ -> Bottom
    | One, Bottom -> Bottom
    | One, One -> Zero
    | One, Plus -> Zero
    | One, Zero -> One
    | One, Minus -> One
    | One, PlusZero -> PlusZero
    | One, MinusZero -> One
    | One, QuestionMark -> PlusZero
    | Plus, Bottom -> Bottom
    | Plus, One -> PlusZero
    | Plus, Plus -> PlusZero
    | Plus, Zero -> One
    | Plus, Minus -> One
    | Plus, PlusZero -> PlusZero
    | Plus, MinusZero -> One
    | Plus, QuestionMark -> PlusZero
    | Zero, Bottom -> Bottom
    | Zero, One -> Zero
    | Zero, Plus -> Zero
    | Zero, Zero -> Zero
    | Zero, Minus -> One
    | Zero, PlusZero -> Zero
    | Zero, MinusZero -> PlusZero
    | Zero, QuestionMark -> PlusZero
    | Minus, Bottom -> Bottom
    | Minus, One -> Zero
    | Minus, Plus -> Zero
    | Minus, Zero -> Zero
    | Minus, Minus -> PlusZero
    | Minus, PlusZero -> Zero
    | Minus, MinusZero -> PlusZero
    | Minus, QuestionMark -> PlusZero
    | PlusZero, Bottom -> Bottom
    | PlusZero, One -> PlusZero
    | PlusZero, Plus -> PlusZero
    | PlusZero, Zero -> PlusZero
    | PlusZero, Minus -> One
    | PlusZero, PlusZero -> PlusZero
    | PlusZero, MinusZero -> PlusZero
    | PlusZero, QuestionMark -> PlusZero
    | MinusZero, Bottom -> Bottom
    | MinusZero, One -> Zero
    | MinusZero, Plus -> Zero
    | MinusZero, Zero -> Zero
    | MinusZero, Minus -> PlusZero
    | MinusZero, PlusZero -> Zero
    | MinusZero, MinusZero -> PlusZero
    | MinusZero, QuestionMark -> PlusZero
    | QuestionMark, Bottom -> Bottom
    | QuestionMark, _ -> PlusZero)
  | Ast.Times -> Error.phase "Sign lattice" "Abstract operator for * not implemented."
  | Ast.Divide -> Error.phase "Sign lattice" "Abstract operator for / not implemented."
  | Ast.Eq -> Error.phase "Sign lattice" "Abstract operator for == not implemented."