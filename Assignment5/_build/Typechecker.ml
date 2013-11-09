open Typed_syntax

module TypeChecker = struct

  exception Type_error of string
   
  module S = Typed_syntax 

  type tenv = (S.id * S.typ) list 

  let lookup (x:S.id) (tenv:tenv) : (S.typ) = 
	try List.assoc x tenv 
	with Not_found -> raise (Type_error ("free identifier " ^ x))

  let extend (x:S.id) (t:S.typ) (tenv:tenv) : (tenv) = 
	(x,t) :: tenv  

  let rec tc (tenv : tenv) (e: S.exp) : S.typ = 
      match e with
        | S.Int e1 -> S.TNum
        | S.Bool e1 -> S.TBool
        | S.Arith (e1,e2,e3) -> (match tc tenv e2, tc tenv e3 with
                                		| S.TNum,S.TNum -> S.TNum
                                		| _ -> raise(Type_error "arithmetic operations expect integer arguements "))
        | S.Cmp (e1,e2,e3) -> (match tc tenv e2 , tc tenv e3 with
                                | S.TNum,S.TNum -> S.TBool
                                | _ ->  raise(Type_error "Comparison operation can be applied only on integers "))
        | S.If (e1,e2,e3) -> (match tc tenv e1 with 
				| S.TBool -> 
					let t2 = tc tenv e2 in  
					let t3 = tc tenv e3 in 
					if t2 = t3 then t2 
					else raise(Type_error " both branches of if must have the same type ") 
				| _ -> raise(Type_error "expected boolean in conditional position"))
	| S.Id e1 -> lookup e1 tenv 					 
	| S.Let (x,e1,e2) -> tc ( extend x (tc tenv e1) tenv ) e2  
	| S.Fun (x,t,e) -> S.TFun (t, tc (extend x t tenv) e)
        | S.Fix (x,t,e) -> let t1 = tc (extend x t tenv) e in 
				if t1 = t then t 
				else raise(Type_error "The paramater applied to the function is of a wrong type") 
        | S.App (e1,e2) -> (match tc tenv e1,tc tenv e2 with 
				|TFun(t1,t2),t1' ->
					if t1 = t1' then t2 
					else raise(Type_error "argument has the wrong type")
				| _ -> raise(Type_error "expected function") ) 
        | S.Empty (t) -> S.TList(t)
        | S.Cons (e1,e2) -> let t = tc tenv e1 in 
				if tc tenv e2 = TList(t) then TList(t) 
				else raise(Type_error "all elements of the list must be of the same type")
        | S.Head (e) -> (match tc tenv e with 
			|TList(t) -> t 
			|_ -> raise(Type_error "Head operation can be applied only to the list")) 
        | S.Tail (e) -> (match tc tenv e with 
			|TList(t) -> t 
			| _ -> raise(Type_error "Tail operation can be applied only to a list")) 
        | S.IsEmpty (e) -> (match tc tenv e with 
			   | TList(t) -> S.TBool 
			   | _ -> raise(Type_error "Is empty operation can be applied only on a list"))
	| S.Pair (e1,e2) -> S.TPair(tc tenv e1,tc tenv e2) 
	| S.ProjL (e) -> (match tc tenv e with 
			   | TPair(t1,t2) -> t1 
			   | _ -> raise(Type_error "Not a pair object to find the left component")) 
			 
	| S.ProjR (e) -> (match tc tenv e with 
			   | TPair(t1,t2) -> t2 
			   | _ -> raise(Type_error "Not a pair object to find the right component"))

  let  type_check (e : S.exp) : S.typ = 
	tc [] e 

  let read_eval (str : string) = match Typed_util.parse str with
  | Typed_util.Exp exp -> type_check exp
  | Typed_util.ParseError msg -> failwith ("parse error: " ^ msg)
  TEST "int"= 
    read_eval "10" = TNum
  TEST "bool"= 
    read_eval "false" = TBool 
  TEST "arith fail" = 
    try 
	let _ = read_eval "4+true" in false 
    with 
    | Type_error "arithmetic operations expect integer arguements " -> true 
  TEST "comp fail " = 
    try 
	let _ = read_eval "true<false" in false 
    with 
    | Type_error "Comparison operation can be applied only on integers " -> true
  TEST "If fail " = 
    try 
	let _ = read_eval " if 10<12 then 10 else true" in false 
    with 
    | Type_error " both branches of if must have the same type " -> true 
  TEST "If fail2 " = 
    try 
	let _ = read_eval " if 10 then 10 else 12" in false 
    with 
    | Type_error "expected boolean in conditional position"-> true
  TEST "Let binding" = 
    try 
       let _ = read_eval " let x = 10 in y+x " in false 
    with 
    | Type_error "free identifier y"->true 
  TEST "Fix pass" = 
    read_eval "fix (self:num->num) -> fun(n:num) -> if n = 0 then 1 else n*self(n - 1)" = TFun(TNum,TNum)   
  TEST "Fix " = 
    try 
       let _ = read_eval " fix(x:num->num)->3" in false 
    with 
    | Type_error "The paramater applied to the function is of a wrong type"->true
  TEST "Function application" = 
    try 
       let _ = read_eval " let f = fun(x:num) -> x+3 in f true" in false 
    with 
    | Type_error "argument has the wrong type" -> true 
  TEST "using functions " = 
    try 
       let _ = read_eval " let f = 10 in f 3" in false 
    with 
    | Type_error "expected function" -> true 
  TEST "Lists " = 
    try 
       let _ = read_eval " 1::2::3::true::empty<num> " in false 
    with 
    | Type_error "all elements of the list must be of the same type" -> true 
  TEST " head " = 
    try 
       let _ = read_eval "head 10" in false
    with 
    | Type_error "Head operation can be applied only to the list" -> true
   TEST "tail" =
     try 
        let _ = read_eval "tail 10" in false 
     with 
     | Type_error "Tail operation can be applied only to a list" -> true 
    TEST "isEmpty" = 
      try 
        let _ = read_eval "empty? 10" in false
      with 
      | Type_error "Is empty operation can be applied only on a list" -> true 
    TEST "pairs" = 
      read_eval "(10,true)" = TPair(TNum,TBool) 
    TEST "Pairs left" = 
       read_eval "(10,true).1" = TNum 
    TEST "Pairs right" = 
       read_eval "(10,true).2" = TBool 
    TEST "left operation" = 
      try 
        let _ = read_eval "let f= fun(x:num)-> x+4 in f.1" in false
      with 
      | Type_error "Not a pair object to find the left component"->true 
    TEST "right operation" =      
       try 
        let _ = read_eval "let f=fun(x:num) -> x+4 in f.2" in false 
       with 
       | Type_error "Not a pair object to find the right component"->true 
  end

module REPL = Typed_eval.Make (TypeChecker)


