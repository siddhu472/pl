
type intlist = 
  | Cons of int * intlist 
  | Empty 

let from_minus_1_to_4 = 
  Cons ( -1 , Cons (0, Cons(1 , Cons(2, Cons(3,Cons(4,Empty))))))


