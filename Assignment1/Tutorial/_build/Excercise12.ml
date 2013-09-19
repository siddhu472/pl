
type intlist = 
 |Empty 
 |Cons of int*intlist 

let rec insert_sorted (n:int) (lst:intlist ) : intlist = 
   match lst with 
     |Empty -> Cons(n,Empty) 
     |Cons(next,rest) ->
         (match n<next with 
            | true -> Cons(n,Cons(next,rest))
            | false -> Cons(next , insert_sorted n rest))

TEST = insert_sorted 5 Empty = Cons (5,Empty) 
TEST = insert_sorted 1 (Cons(0,Cons(2,Empty))) = Cons(0,Cons(1,Cons(2,Empty)))
TEST = insert_sorted 10 (Cons(10,Empty)) = Cons(10,Cons(10,Empty))       
