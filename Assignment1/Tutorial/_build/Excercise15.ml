type exp = 
  | Int of int 
  | Add of exp*exp 
  | Mul of exp*exp 

let  rec eval (e:exp) : int = 
  match e with 
  | Int n->n 
  | Add (e1,e2) -> eval e1 + eval e2 
  | Mul (e1,e2) -> eval e1 * eval e2 

TEST = eval (Add (Int 10,Int 5)) = 15 
TEST = eval (Mul(Add(Int 2,Int 3),Int 5)) = (2+3)*5 
