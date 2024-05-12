type exp =
  | Zero
  | One
  | Char of char
  | Sum of exp * exp
  | Prod of exp * exp
  | Star of exp
