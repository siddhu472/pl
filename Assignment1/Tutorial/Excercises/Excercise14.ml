
type exp = 
 | Int of int 
 | Add of exp*exp 
 | Mul of exp*exp 

let ex1 = Add(Int 10,Int 5) 
let ex2 = Mul(Add (Int 2,Int 3),Int 5) 
let ex3 = Mul((Mul (Int 3,Int 0)),Mul (Int 3,Int 5))


