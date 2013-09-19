
type intlist = 
  | Cons of int*intlist 
  | Empty 

let rec all_positive ( l : intlist ) : bool = 
  match l with 
   | Empty -> true 
   | Cons (x,r) -> x > 0 && all_positive r 

TEST = all_positive (Cons(1,Empty)) = true 
TEST = all_positive (Cons(-1,Cons(1,Empty))) = false

