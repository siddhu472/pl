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

let rec insertion_sort (lst:intlist) :intlist = 
 match lst with 
    |Empty -> Empty 
    |Cons(n,rest) -> insert_sorted n (insertion_sort rest)

TEST =  insertion_sort (Cons(3,Cons(2,Cons(1,Empty)))) = Cons(1,Cons(2,Cons(3,Empty)))
