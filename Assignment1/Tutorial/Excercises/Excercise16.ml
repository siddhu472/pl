type exp =
  | Int of int
  | Add of exp*exp
  | Mul of exp*exp

let  rec print (e:exp) : string =
  match e with
  | Int n-> string_of_int n
  | Add (e1,e2) -> "(" ^ print e1 ^ "+" ^ print e2 ^ ")"
  | Mul (e1,e2) -> "(" ^ print e1 ^ "*" ^ print e2 ^ ")"

TEST = print (Add (Int 10,Int 5)) = "(10+5)"
TEST = print (Mul(Add(Int 2,Int 3),Int 5)) = "((2+3)*5)"
