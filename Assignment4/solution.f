
let triple = typfun u -> typfun v -> typfun w -> fun(x:u)->fun(y:v)->fun(z:w)->typfun R -> fun(p:u->v->w->R)-> p x y z;;

type [[Triple U V W]] = forall R. (U-> V -> W -> R) -> R;;

let triple_proj1 = typfun T1 -> typfun T2 -> typfun T3 -> fun(v: [[Triple T1 T2 T3]])-> v <T1> (fun(x:T1) -> fun(y:T2) -> fun(z:T3) -> x );;

let triple_proj2 = typfun T1 -> typfun T2 -> typfun T3 -> fun(v: [[Triple T1 T2 T3]])-> v <T2> (fun(x:T1) -> fun(y:T2) -> fun(z:T3) -> y );;

let triple_proj3 = typfun T1 -> typfun T2 -> typfun T3 -> fun(v: [[Triple T1 T2 T3]])-> v <T3> (fun(x:T1) -> fun(y:T2) -> fun(z:T3) -> z );;


type [[Bool]] = forall R . R -> R -> R;;

let true = typfun R -> fun (x : R) -> fun (y : R) -> x;;

let false = typfun R -> fun (x : R) -> fun (y : R) -> y;;

let if = typfun R -> 
  fun (cond : [[Bool]]) ->
    fun (true_branch : R) ->
      fun (false_branch : R) ->
        cond<R> true_branch false_branch;;

let and =
  fun (e1 : [[Bool]]) ->
    fun (e2 : [[Bool]]) ->
      typfun R ->
        fun (x : R) ->
          fun (y : R) ->
            e1 <R> (e2 <R> x y) y;;

let or =
  fun (e1 : [[Bool]]) ->
    fun (e2 : [[Bool]]) ->
      typfun R ->
        fun (x : R) ->
          fun (y : R) ->
            e1 <R> x (e2 <R> x y);;

let not = 
   fun (e1:[[Bool]]) -> 
      typfun R -> 
          fun (x : R) -> 
             fun ( y : R) -> 
                 e1 <R> y x ;;    


let nand = 
   fun(e1:[[Bool]])-> 
     fun(e2:[[Bool]])-> 
        typfun R-> 
          fun (x : R)-> 
            fun(y :R) -> 
              e1 <R> y ( e2<R> y x) ;; 

let xor  = 
   fun(e1:[[Bool]]) ->
      fun(e2:[[Bool]])-> 
         typfun R -> 
           fun (x:R) -> 
             fun (y:R) -> 
                e1 <R> ( e2 <R> y x ) ( e2<R> x y ) ;; 


type [[foo T]] = forall R.(T->T->R)->R->R ;; 

let bar = typfun T -> 
            fun(x:T) -> fun(y:T) -> 
                typfun R -> 
                   fun(bar: T->T->R) -> 
                      fun(baz:R)-> 
                          bar x y ;;

let baz = typfun T -> 
           typfun R -> 
               fun(bar :T->T->R)-> 
                  fun(baz:R) -> 
                     baz ;;

let foo_proj1 = typfun T -> 
                    fun(v:[[foo T]])-> 
                        fun(baz:T)->
                             v <T> (fun(x:T)->fun(y:T)-> x) baz  ;;   

let foo_proj2 = typfun T -> 
                     fun(v:[[foo T]])-> 
                         fun(baz:T) -> 
                             v <T> (fun(x:T)->fun(y:T)->y) baz ;; 

type [[option T]] = forall R. (T->R)->R->R ;; 

let some = typfun T -> fun(x:T)-> typfun R -> 
   fun (some :T->R) -> fun(none:R) ->
        some x ;; 

let none = typfun T -> typfun R -> 
            fun (some:T->R) -> fun (none:R) -> none ;; 

let option_case = typfun T -> typfun R -> 
    fun ( v:[[option T]])-> 
     fun(some : T->R) -> 
         fun(none : R) -> 
             v <R> some none 
;;

type [[List T]] = forall R . (T -> R -> R) -> R -> R;;

let cons = typfun T -> fun (hd : T) ->
  fun (tl : [[List T]]) ->
  typfun R -> 
    fun (c : T -> R -> R) -> fun (n : R) ->
    c hd (tl <R> c n);;

let empty = typfun T -> 
  typfun R ->
    fun (c : T -> R -> R) ->
      fun (n : R) ->
        n;;

let fold_right = typfun A -> typfun B -> 
                    fun(f:A->B->B) -> fun(lst :[[List A]]) -> fun(init:B) -> 
                       lst <B> f init 
;;
let snoc = typfun A ->
              fun (x:A) ->  
              fun (lst:[[List A]]) -> 
                     fold_right <A> <[[List A]]>( fun(hd:A) -> fun(tl:[[List A]]) -> 
                                            cons<A> hd tl ) lst (cons <A> x (empty <A>))
;;
let rev = typfun A -> 
             fun (lst:[[List A]]) -> 
                  fold_right <A> <[[List A]]> (snoc <A>) lst (empty<A>)         
;;

type [[Pair T]] = forall R.(T->T->R)->R ;;

let parts = typfun T ->
            fun(x:T) -> fun(y:T) ->
                typfun R ->
                   fun(p: T->T->R) ->
                          p x y ;;


let pair_proj1 = typfun T ->
                    fun(v:[[Pair T]])->
                             v <T> (fun(x:T)->fun(y:T)-> x);;

let pair_proj2 = typfun T ->
                     fun(v:[[Pair T]])->
                             v <T> (fun(x:T)->fun(y:T)->y);;


let insert =fun(n:int) ->  fun(lst:[[List int]]) -> 
                   fold_right <int> <[[Pair [[List int]]]]> (fun(m:int)->fun(p:[[Pair [[List int]]]]) ->
        if  <[[Pair [[List int]]]]> (n>m) 
(parts <[[List int]]> (cons <int> m (pair_proj1 <[[List int]]> p)) (cons <int> m (pair_proj2 <[[List int]]> p)))
(parts <[[List int]]> (cons <int> n ( cons <int> m (pair_proj2 <[[List int]]> p))) (cons <int> m (pair_proj2 <[[List int]]> p)))
)lst (parts <[[List int]]> (cons <int> n (empty<int>)) (empty<int>) )

;; 


let insertion_sort = fun(lst:[[List int]]) -> 
                       fold_right <int> <[[List int]]> (fun(hd:int) -> fun(tl:[[List int]]) -> pair_proj1 <[[List int]]> (insert hd tl) ) lst (empty<int>);;  


type [[BTree T]] = forall R. (T->R->R->R) -> R -> R 

;;

let node = typfun T ->fun(v:T) -> 
                       fun (l:[[BTree T]]) ->
                         fun(r:[[BTree T]]) -> 
                    typfun R -> fun(t :T->R->R->R) -> fun(n:R) -> 
                         t v (l <R> t n) (r <R> t n) 
;;   
 
let nil  = typfun T -> 
              typfun R ->
                  fun(t:T->R->R->R) -> fun(n:R) -> 
                        n;;

let tree_fold = typfun A -> typfun B -> 
                   fun(f:A->B->B->B) -> fun(x:[[BTree A]]) -> fun(init:B) -> 
                     x <B> f init  ;;  

let tree_insert_internal = fun(n:int) -> fun(t:[[BTree int]]) -> 
tree_fold <int> <[[Pair [[BTree int]]]]> ( fun(m:int) -> fun(l:[[Pair [[BTree int]]]]) -> fun(r:[[Pair [[BTree int]]]]) ->
 if <[[Pair [[BTree int]]]]> (n<m)
(parts<[[BTree int]]> 
(node <int> m (pair_proj1 <[[BTree int]]> l) (pair_proj2 <[[BTree int]]> r))
(node <int> m (pair_proj2 <[[BTree int]]> l) (pair_proj2 <[[BTree int]]> r))
)
(
if <[[Pair [[BTree int]]]]> (n>m)
(
parts<[[BTree int]]> 
(node <int> m (pair_proj2 <[[BTree int]]> l) (pair_proj1 <[[BTree int]]> r))
(node <int> m (pair_proj2 <[[BTree int]]> l) (pair_proj2 <[[BTree int]]> r))
)
(
parts<[[BTree int]]>
(node <int> m (node <int> n (pair_proj2 <[[BTree int]]> l) (nil<int>) ) (pair_proj2 <[[BTree int]]> r))
(node <int> m (pair_proj2 <[[BTree int]]> l)(pair_proj2 <[[BTree int]]> r))
)
)
) t (parts <[[BTree int]]> (node <int> n (nil<int>) (nil<int>)) (nil <int>))  

;;

let tree_insert = fun(n:int) -> fun(t:[[BTree int]]) -> pair_proj1 <[[BTree int]]> (tree_insert_internal n t) 

;;

let tree_find = fun(n:int) -> fun(t:[[BTree int]]) -> 
tree_fold <int> <[[Bool]]> (fun (m:int) -> fun (l:[[Bool]]) -> fun(r:[[Bool]]) -> 
if <[[Bool]]> (n = m) 
(true)
(if <[[Bool]]> (n<m) (l) (r))
) t false  

;;

let x = triple <int> <int> <int> 1 2 3 ;; 
triple_proj1 <int> <int> <int>  x;;
triple_proj2 <int> <int> <int>  x;;
triple_proj3 <int> <int> <int>  x;;

not true <int >  1 2 ;; 
nand true true <int >  1 2 ;; 
xor true true <int >  1 2 ;; 

let a = bar <int> 1 2;; 
let b = baz <int> ;; 
foo_proj1 <int> a 3 ;; 
foo_proj2 <int> a 3 ;; 
foo_proj1 <int> b 3 ;; 
foo_proj2 <int> b 3 ;; 

let exlist = cons <int> 100 ( cons <int> 200 ( cons <int> 150 (empty <int>)));;
rev <int> exlist ;; 

let exlist2 = cons <int> 100 ( cons <int> 200 ( cons <int> 150 ( cons<int> 10 (empty <int>)))) ;; 
insertion_sort exlist2 ;; 

let extree = node <int> 10 ( node <int> 5 ( nil <int>) (nil<int>) ) ( node <int> 15 (nil<int>) (nil<int>)) ;; 
tree_insert 100 extree ;; 

tree_find 10 extree <int> 1 2 ;;  

