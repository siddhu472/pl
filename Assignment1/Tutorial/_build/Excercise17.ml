type exp =
  | Int of int
  | Add of exp * exp
  | Mul of exp * exp

type cxt = 
  | TopCxt
  | MulCxt 
  | AddCxt

let rec pp (enclosing: cxt) (e:exp) : string = 
  match e with 
   | Int n -> string_of_int n 
   | Add(e1,e2) -> 
     let inner = ( pp AddCxt e1) ^ " + " ^ (pp AddCxt e2 ) in 
      ( match enclosing with 
        | MulCxt -> "(" ^ inner ^ ")" 
        | _ -> inner ) 
   | Mul(e1,e2) -> 
       ( pp MulCxt e1 ) ^ " * " ^ ( pp MulCxt e2) 

let pretty_print (e : exp) : string = pp TopCxt e 
 
TEST = pretty_print ( Add (Int 10,Int 5)) = "10 + 5" 
TEST = pretty_print ( Mul ( Add ( Int 2 , Int 3) , Int 5)) = "(2 + 3) * 5" 
TEST = pretty_print (Mul ((Mul (Int 3,Int 0)),Mul(Int 3,Int 5))) = "3 * 0 * 3 * 5" 
