module I = Typeinf_syntax.Implicit 
module E = Typeinf_syntax.Explicit 

open E 

(*1. placeholder *)

let rec convertAST (exp:I.exp) : E.exp =
	match exp with 
 	| I.Int x -> E.Int x  
	| I.Bool x -> E.Bool x 
	| I.Arith (op,e1,e2) -> E.Arith(op,convertAST e1,convertAST e2)
	| I.Cmp(op,e1,e2) -> E.Cmp (op,convertAST e1,convertAST e2) 
	| I.If(e1,e2,e3) -> E.If (convertAST e1,convertAST e2,convertAST e3) 
	| I.Id i -> E.Id i 
	| I.Let (i,e1,e2) -> E.Let (i,convertAST e1,convertAST e2) 
	| I.Fun (i,e1) -> E.Fun (i,E.TId (Identifier.fresh "fun"),convertAST e1) 
	| I.Fix (i,e1) -> E.Fix (i,E.TFun(E.TId (Identifier.fresh "fix1"),E.TId (Identifier.fresh "fix2")),convertAST e1) 
	| I.App (e1,e2) -> E.App(convertAST e1,convertAST e2) 
	| I.Empty  (* Type t *) -> E.Empty (E.TId (Identifier.fresh "empty"))
	| I.Cons (e1,e2) -> E.Cons (convertAST e1,convertAST e2) 
	| I.Head e1 -> E.Head(convertAST e1) 
	| I.Tail e1 -> E.Tail(convertAST e1) 
	| I.IsEmpty e1 -> E.IsEmpty (convertAST e1) 
	| I.Pair (e1,e2) -> E.Pair(convertAST e1,convertAST e2)
	| I.ProjL e1 -> E.ProjL (convertAST e1) 
	| I.ProjR e1 -> E.ProjR (convertAST e1) 


(* 2.Constraint generation *)

type env = (Typeinf_syntax.id*E.typ) list 
type constraints = (E.typ * E.typ)   
let cs :  constraints list ref = ref [] 

let extend (x:Typeinf_syntax.id) (t:E.typ) (tenv:env) : (env) =
        (x,t) :: tenv

let lookup (env:env) (id:Identifier.t) : E.typ = 
	try List.assoc id env
        with Not_found -> failwith ("Free identifier ")

let rec cgen (env:env) (exp:E.exp) : E.typ = 
	match exp with 
	 | E.Int x -> cs := (E.TInt , E.TInt)::!cs ;  E.TInt 
	 | E.Bool x -> cs := (E.TBool , E.TBool)::!cs ; E.TBool 
	 | E.Arith (op,e1,e2) -> cs:=(cgen env  e1,E.TInt)::(cgen env e2,E.TInt)::!cs ; E.TInt 
	 | E.Cmp (op,e1,e2) -> cs:=(cgen env e1,E.TInt) ::(cgen env e2,E.TInt)::!cs ; E.TBool 
	 | E.If(e1,e2,e3) -> let t1 = cgen env e2 in let t2 = cgen env e3 in 
				cs:=(cgen env e1,E.TBool)::(t1,t2)::!cs ; t1
	 | E.Id i -> lookup env i
	 | E.Let (id,e1,e2) -> let t1 = cgen env e1 in let t2 = cgen (extend id t1 env) e2 in t2   
	 | E.Fun (id,t,e1) -> let t1 = cgen (extend id t env ) e1  in E.TFun(t,t1)  
         | E.Fix (id,t,e1) -> let t1 = cgen (extend id t env ) e1 in cs:=(t,t1)::!cs ; E.TFun(t,t1) 
	 | E.App(e1,e2) -> let t1 = cgen env e1 in 
				let t2 = cgen env e2 in 
					let t3 = E.TId (Identifier.fresh "app") in 
							cs:=(t1,E.TFun(t2,t3))::!cs;t3 
	 | E.Empty t -> E.TList(t)  
	 | E.Cons (e1,e2) ->let h = cgen env e1 in let t = cgen env e2 in  
                      cs:=(E.TList(h) , t)::!cs ; E.TList(h)
	 | E.Head (e1) ->  let t1 = cgen env e1 
				in let t2 = E.TId (Identifier.fresh "head") in 
					cs:= (t1,TList(t2))::!cs ; t2  
	 | E.Tail (e1) ->  let t1 = cgen env e1 
				in let t2 = E.TId (Identifier.fresh "tail") in 
					cs := (t1,TList(t2))::!cs ; t1
         | E.IsEmpty (e1) -> let t1 = cgen env e1  
				in let t2 = E.TId ( Identifier.fresh "empty") in 
					cs := (t1,TList(t2))::!cs ; E.TBool 
	 | E.Pair(e1,e2) -> E.TPair(cgen env e1,cgen env e2)
	 | E.ProjL e1 -> let t1 = cgen env e1 in 
				let t2 = E.TId (Identifier.fresh "projl") in 
					let t3 = E.TId (Identifier.fresh "projr") in 
						cs :=(t1,TPair(t2,t3))::!cs; t2   
	 | E.ProjR e1 -> let t1 = cgen env e1 in 
				let t2 = E.TId (Identifier.fresh "projl") in 
					let t3 = E.TId (Identifier.fresh "projr") in 
						cs := (t1,TPair(t2,t3))::!cs ; t3 


(*3. Substitution *)

module type SUBST  = sig 
	type t 
	val empty : t 
	val singleton :Typeinf_syntax.id -> E.typ -> t 
	val apply : t->E.typ -> E.typ 
	val compose : t-> t -> t 
	val to_list : t-> (Typeinf_syntax.id * E.typ ) list  
end 

module Subst : SUBST = struct 
        module IdMap = Map.Make (Identifier)
        type t = Typeinf_syntax.Explicit.typ IdMap.t

	let empty : t = IdMap.empty 
  
	let singleton (x:Typeinf_syntax.id) (typ:E.typ) : t =  
		IdMap.singleton x typ   

        let  rec apply (map:t) (typ:E.typ) : E.typ = 
		(match typ with 
		  | E.TId x ->	(try 
				 IdMap.find x map
				with 
				| Not_found -> typ)  
		  | E.TInt -> E.TInt
		  | E.TBool -> E.TBool 
		  | E.TFun (t1,t2) -> E.TFun(apply map t1,apply map t2)
		  | E.TPair (t1,t2) -> E.TPair(apply map t1,apply map t2) 
		  | E.TList (t1) -> E.TList(apply map t1) ) 

		 	
		
	let compose (map1:t) (map2:t) : t = 
		IdMap.merge (fun k m1 m2 ->match m1 , m2 with
                 | Some x , Some y -> if x = y then Some x else failwith("Both maps have the same keys")
                 | None , Some y -> Some (apply map1 y) 
                 | Some x , None -> Some x
		 | None , None -> failwith(" Both maps dont have the keys ") 
                )map1 map2  
			 
	let to_list (map:t) : (Identifier.t*E.typ) list = 
		IdMap.bindings map 
		 
end
  
(*4. Unification *)

let rec occurs (x:Identifier.t) (t:E.typ) : bool = 
	match t with 
	| TId y -> if x = y then true else false 
	| TInt -> false 
	| TBool -> false 
	| TFun (t1,t2) -> (occurs x t1) or (occurs x t2)  
	| TPair (t1,t2) -> (occurs x t1) or (occurs x t2) 
	| TList (t1) -> occurs x t1 

let rec unify (t1:E.typ) (t2:E.typ) : Subst.t = 
	match t1,t2 with 
	| E.TInt , E.TInt -> Subst.empty 
	| E.TBool , E.TBool -> Subst.empty
	| E.TFun(t3,t4) , E.TFun(t5,t6)-> let s1 = (unify t3 t5) in 
					  	let s2 = unify ( Subst.apply s1 t4 ) (Subst.apply s1 t6) in (Subst.compose s2 s1) 
	| E.TPair(t3,t4) , E.TPair(t5,t6) -> let s1 = unify t3 t5 in 
						let s2 = unify (Subst.apply s1 t4 )( Subst.apply s1 t6) in (Subst.compose s2 s1) 
	| E.TList(t3),E.TList(t4) -> unify t3 t4 
	| E.TId x , _ -> if t2 = t1 then Subst.singleton x t2 else ( if occurs x t2 = false then Subst.singleton x t2 else failwith ("occurs check failed") ) 
	| _ , E.TId x -> if t1 = t2 then Subst.singleton x t2 else (if occurs x t1 = false then Subst.singleton x t1 else failwith ("occurs check failed")) 
	| _,_-> failwith("unification failed") 	 

(*Solver*)
	 
let rec apply_helper (map : Subst.t) (x:((E.typ*E.typ) list)) : ((E.typ*E.typ) list) = 
	match x with 
	| (x1,x2)::rest -> ((Subst.apply map x1 ),(Subst.apply map x2)) :: apply_helper map rest   
	| [] -> []  	

let rec solver (x:((E.typ*E.typ) list)) : Subst.t = 
	match x with 	
	| (x1,x2) :: rest -> let s1 = (unify x1 x2) in let  x= (apply_helper s1 rest) in Subst.compose (solver x) s1
	| [] -> Subst.empty 

(*annotate *)
	
let  rec annotate_exp (map:Subst.t) (exp:E.exp) : E.exp = 
	match exp with 
	| E.Int x -> E.Int x
        | E.Bool x -> E.Bool x
        | E.Arith (op,e1,e2) -> E.Arith(op,annotate_exp map e1,annotate_exp map e2)
        | E.Cmp(op,e1,e2) -> E.Cmp (op,annotate_exp map e1,annotate_exp map e2)
        | E.If(e1,e2,e3) -> E.If (annotate_exp map e1,annotate_exp map e2,annotate_exp map e3)
        | E.Id i -> E.Id i 
        | E.Let (i,e1,e2) -> E.Let (i,annotate_exp map e1,annotate_exp map e2)
        | E.Fun (i,t,e1) -> E.Fun (i,Subst.apply map t,annotate_exp map e1)
        | E.Fix (i,t,e1) -> E.Fix (i,Subst.apply map t,annotate_exp map e1)
        | E.App (e1,e2) -> E.App(annotate_exp map e1,annotate_exp map e2)
        | E.Empty t -> E.Empty (Subst.apply map t)
        | E.Cons (e1,e2) -> E.Cons (annotate_exp map e1,annotate_exp map e2)
        | E.Head e1 -> E.Head(annotate_exp map e1)
        | E.Tail e1 -> E.Tail(annotate_exp map e1)
        | E.IsEmpty e1 -> E.IsEmpty (annotate_exp map e1)
        | E.Pair (e1,e2) -> E.Pair(annotate_exp map e1,annotate_exp map e2)
        | E.ProjL e1 -> E.ProjL (annotate_exp map e1)
        | E.ProjR e1 -> E.ProjR (annotate_exp map e1) 


module TypeChecker = struct 

exception Type_error of string

let rec tc (tenv : env) (e:exp) : typ = 
      match e with
        | Int e1 -> TInt 
        | Bool e1 -> TBool
        | Arith (e1,e2,e3) -> (match tc tenv e2, tc tenv e3 with
                                		| TInt,TInt -> TInt
                                		| _ -> raise(Type_error "arithmetic operations expect integer arguements "))
        | Cmp (e1,e2,e3) -> (match tc tenv e2 , tc tenv e3 with
                                | TInt,TInt -> TBool
                                | _ ->  raise(Type_error "Comparison operation can be applied only on integers "))
        | If (e1,e2,e3) -> (match tc tenv e1 with 
				| TBool -> 
					let t2 = tc tenv e2 in  
					let t3 = tc tenv e3 in 
					if t2 = t3 then t2 
					else raise(Type_error " both branches of if must have the same type ") 
				| _ -> raise(Type_error "expected boolean in conditional position"))
	| Id e1 -> lookup  tenv e1					 
	| Let (x,e1,e2) -> tc ( extend x (tc tenv e1) tenv ) e2  
	| Fun (x,t,e) -> TFun (t, tc (extend x t tenv) e)
        | Fix (x,t,e) -> let t1 = tc (extend x t tenv) e in 
				if t1 = t then t 
				else raise(Type_error "The paramater applied to the function is of a wrong type") 
        | App (e1,e2) -> (match tc tenv e1,tc tenv e2 with 
				|TFun(t1,t2),t1' ->
					if t1 = t1' then t2 
					else raise(Type_error "argument has the wrong type")
				| _ -> raise(Type_error "expected function") ) 
        | Empty (t) -> TList(t)
        | Cons (e1,e2) -> let t = tc tenv e1 in 
				if tc tenv e2 = TList(t) then TList(t) 
				else raise(Type_error "all elements of the list must be of the same type")
        | Head (e) -> (match tc tenv e with 
			|TList(t) -> t 
			|_ -> raise(Type_error "Head operation can be applied only to the list")) 
        | Tail (e) -> (match tc tenv e with 
			|TList(t) -> t 
			| _ -> raise(Type_error "Tail operation can be applied only to a list")) 
        | IsEmpty (e) -> (match tc tenv e with 
			   | TList(t) -> TBool 
			   | _ -> raise(Type_error "Is empty operation can be applied only on a list"))
	| Pair (e1,e2) -> TPair(tc tenv e1,tc tenv e2) 
	| ProjL (e) -> (match tc tenv e with 
			   | TPair(t1,t2) -> t1 
			   | _ -> raise(Type_error "Not a pair object to find the left component")) 
			 
	| ProjR (e) -> (match tc tenv e with 
			   | TPair(t1,t2) -> t2 
			   | _ -> raise(Type_error "Not a pair object to find the right component"))

  let  type_check (e : exp) : typ = 
	tc [] e 

end 


(*Type inference*)

let typeinf (exp:I.exp) : E.exp = 
	let x = convertAST(exp)	in 
		let _ = cgen [] x in 
		  	let z = solver (!cs) in 
				let output = annotate_exp z x in TypeChecker.type_check (output);output 
  
let rec repl () =
  print_string "> ";
  match Typeinf_util.parse (read_line ()) with
  | Typeinf_util.Exp exp ->
    let v = (typeinf(exp)) in
    print_string (Typeinf_util.string_of_exp v); 
    print_newline ();
    repl ()
  | Typeinf_util.ParseError msg ->
    print_string msg;
    print_newline ();
    repl ()
let _ =
  match Array.to_list Sys.argv with
  | [ exe; "repl" ] -> print_string "Press Ctrl + C to quit.\n"; repl ()
  | _ -> ()

let read_eval (str : string) = match Typeinf_util.parse str with
  | Typeinf_util.Exp exp -> typeinf exp
  | Typeinf_util.ParseError msg -> failwith ("parse error: " ^ msg)

let x = Identifier.fresh "x"
let y = Identifier.fresh "y"

TEST "Subst.apply should replace x with TInt" =
  let s = Subst.singleton x E.TInt in
  Subst.apply s (E.TId x) = E.TInt

TEST "Subst.apply should recur into type constructors" =
  let s = Subst.singleton x E.TInt in
  Subst.apply s (E.TFun (E.TId x, E.TBool)) = (E.TFun (E.TInt, E.TBool))

TEST "Subst.compose should distribute over Subst.apply (1)" =
  let s1 = Subst.singleton x E.TInt in
  let s2 = Subst.singleton y E.TBool in
  Subst.apply (Subst.compose s1 s2) (E.TFun (E.TId x, E.TId y)) =
  Subst.apply s1 (Subst.apply s2 (E.TFun (E.TId x, E.TId y)))

TEST "Subst.compose should distribute over Subst.apply (2)" =
  let s1 = Subst.singleton x E.TBool in
  let s2 = Subst.singleton y (E.TId x) in
  Subst.apply (Subst.compose s1 s2) (E.TFun (E.TId x, E.TId y)) =
  Subst.apply s1 (Subst.apply s2 (E.TFun (E.TId x, E.TId y)))

TEST "unifying identical base types should return the empty substitution" =
  Subst.to_list (unify TInt TInt) = []

TEST "unifying distinct base types should fail" =
  try let _ = unify TInt TBool in false
  with Failure "unification failed" -> true

TEST "unifying with a variable should produce a singleton substitution" =
  let x = Identifier.fresh "myvar" in
  Subst.to_list (unify TInt (TId x)) = [(x, TInt)]

TEST "unification should recur into type constructors" =
  let x = Identifier.fresh "myvar" in
  Subst.to_list (unify (TFun (TInt, TInt)) 
                       (TFun (TId x, TInt))) = 
  [(x, TInt)]

TEST "unification failures may occur across recursive cases" =
  try
    let x = Identifier.fresh "myvar" in  
    let _ = unify (TFun (TInt, TId x)) 
                  (TFun (TId x, TBool)) in
    false
  with Failure "unification failed" -> true

TEST "unification should produce a substitution that is transitively closed" =
  let x = Identifier.fresh "myvar1" in  
  let y = Identifier.fresh "myvar2" in  
  let z = Identifier.fresh "myvar3" in  
  let subst = unify (TFun (TFun (TInt, TId x), TId y))
                    (TFun (TFun (TId x, TId y), TId z)) in
  Subst.to_list subst = [ (x, TInt); (y, TInt); (z, TInt) ]

TEST "unification should detect constraint violations that require transitive
      closure" =
  try
    let x = Identifier.fresh "myvar1" in  
    let y = Identifier.fresh "myvar2" in  
    let _ = unify (TFun (TFun (TInt, TId x), TId y))
                      (TFun (TFun (TId x, TId y), TBool)) in
    false
  with Failure "unification failed" -> true

TEST "unification should implement the occurs check (to avoid infinite loops)" =
  try
    let x = Identifier.fresh "myvar" in  
    let _ = unify (TFun (TInt, TId x)) (TId x) in
    false (* a bug is likely to cause an infinite loop *)
  with Failure "occurs check failed" -> true

TEST "unification on Lists (1) " = 
     let x = Identifier.fresh "x" in 
     	let subst = unify (TList (TId x)) (TList (TInt)) in 
		Subst.to_list subst = [ (x,TInt) ] 

TEST "unification of Lists (2) " = 
     let x = Identifier.fresh "x" in
     let y = Identifier.fresh "y" in  
	let subst = unify (TList (TList (TId x))) (TList (TId y)) in 
		Subst.to_list subst = [ (y,TList(TId x)) ] 

TEST "unification of Lists (3) " = 
    try 
     let x = Identifier.fresh "x" in 
     let _ = unify (TList (TList (TId x))) (TInt) in 
     false 
    with Failure "unification failed" -> true 
    		
TEST "unification on Pair (1) " = 
	let x = Identifier.fresh "x" in 
	let y = Identifier.fresh "y" in 
	let z = Identifier.fresh "z" in 
	let subst = unify (TPair ( TPair ( TInt , TId x),TId y)) 
			  (TPair ( TPair ( TId x , TId y),TId z)) in 
	Subst.to_list subst = [ (x,TInt); (y,TInt) ; (z,TInt) ] 

TEST "unification on recursive Pairs (2) " =
  try
    let x = Identifier.fresh "myvar" in
    let _ = unify (TPair (TInt, TId x))
                  (TPair (TId x, TBool)) in
    false
  with Failure "unification failed" -> true

TEST "unification should implement the occurs check (to avoid infinite loops) (3)" =
  try
    let x = Identifier.fresh "myvar" in
    let _ = unify (TPair (TInt, TId x)) (TId x) in
    false (* a bug is likely to cause an infinite loop *)
  with Failure "occurs check failed" -> true



TEST "fix " =  
   let expr = "fix self -> fun x -> if x = 0 then 1 else ( x + self ( x - 1))" in
	let result = read_eval expr in 
		Typeinf_util.string_of_exp result = "fix (self : int -> int) ->
  fun (x : int) -> if x = 0] then 1 else x + self (x - 1)" 

TEST "occurence case " = 
   let expr = "let f = fun x -> if true then (x,x) else (x,x) in f 2" in 
      let result = read_eval expr in 
		Typeinf_util.string_of_exp result =  "let f = fun (x : int) -> if true then ((x, x)) else ((x, x)) in f 2"

TEST "Projection test"  = 
   let expr = "let f = fun x -> x.1 in f (1,true)" in 
	let result = read_eval expr in 
		Typeinf_util.string_of_exp result = "let f = fun (x : (int, bool)) -> x.1 in f ((1, true))" 

TEST "head test" = 
   let expr = "let f = fun x -> head x in f (1::2::empty) " in 
	let result = read_eval expr in 
		Typeinf_util.string_of_exp result = "let f = fun (x : int list) -> head x in f (1 :: 2 :: empty<int>)"

TEST "Combining types " = 
   let expr = "let f = fun x -> fun y -> x.1 ::  y in f (1,2) (1::2::empty) " in  
	let result = read_eval expr in 
		Typeinf_util.string_of_exp result = "let f =
      fun (x : (int, int)) ->
        fun (y : int list) -> x.1 :: y in
        f ((1, 2)) (1 :: 2 :: empty<int>)"

TEST "failing test (1) " =
  try    
  let expr = "let f = fun t -> fun r -> if true then t else r in f 1 true" in 
  	let _ = read_eval expr in false 
  with 
    | Failure "unification failed" -> true 

TEST "failing test (2) " =
  try
  let expr = "let f = fun x -> fun y ->  x :: y :: empty in f 1 true" in
        let _ = read_eval expr in false
  with
    | Failure "unification failed" -> true

