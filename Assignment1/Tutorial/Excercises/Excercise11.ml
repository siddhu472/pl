
type intlist = 
  |Empty 
  |Cons of int*intlist 

let rec is_sorted (lst :intlist ) : bool = 
  match lst with 
   | Empty -> true 
   | Cons(r,rest) -> match rest with 
       | Cons(x,others) -> r<=x && is_sorted(rest)
       | Empty -> true 

TEST = is_sorted(Cons (1,Cons(2,Cons(3,Empty)))) = true 
TEST = is_sorted(Cons (1,Cons(4,Cons(2,Empty)))) = false 
