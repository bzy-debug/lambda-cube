type exp =
  | Var of string
  | Abs of string * exp
  | App of exp * exp
[@@deriving show]
