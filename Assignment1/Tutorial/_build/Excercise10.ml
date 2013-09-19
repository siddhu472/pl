
type intlist = 
  |Empty 
  |Cons of int*intlist 

let rec all_even (lst:intlist) : bool = 
  match lst with 
   |Empty -> true 
   |Cons(x,r) -> x mod 2 = 0 && all_even r 

TEST = all_even (Cons(2,Empty)) = true 
TEST = all_even (Cons(1,Cons(3,Cons(2,Empty)))) = false 
TEST = all_even (Empty) = true 
