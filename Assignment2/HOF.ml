(* Type definition *)

type value =
    | IntVal of int  
    | Funv of (HOF_syntax.id) list *HOF_syntax.exp*env
    | RecordV of (HOF_syntax.field * HOF_syntax.exp)  list 
and env = (HOF_syntax.id*value) list


(* Helper Functions *)

let to_string(v:value) : string =
   match v with
   | IntVal n -> string_of_int n
   | _  -> failwith ("expected string")

let to_int(v:value) : int = 
   match  v with 
       | IntVal n-> n 
       |_ ->  failwith (" Expected Integer ") 

let to_string_HOF(i:HOF_syntax.exp) :string = 
   match i with 
      | HOF_syntax.Int i-> string_of_int i 
      | _ -> failwith (" Expected Integer ") 


let rec lookup(id:HOF_syntax.id)(binds:env) : value = 
    match binds with 
      | [] -> failwith ("Free string ")  
      |(x,v)::rest -> if x = id then v 
         else lookup id rest 



let rec set_field  (f:HOF_syntax.field) (rec_list:(HOF_syntax.field*HOF_syntax.exp) list) (new_exp : HOF_syntax.exp): (HOF_syntax.field*HOF_syntax.exp) list  = 
     match rec_list with 
     | (x,v)::rest -> if x = f then  (x,new_exp)::rest 
                               else (set_field f rest new_exp )
     | _ -> failwith("record not found") 

and 

 lookup_records (f:HOF_syntax.field) (rec_list:(HOF_syntax.field*HOF_syntax.exp) list) : value = 
     match rec_list with 
     | (x,v)::rest ->   if x = f then  eval [] v  
                      else  lookup_records f rest
     | _ -> failwith("record not found")

and 

 apply_function (id_list:(HOF_syntax.id)list) (e_list:(HOF_syntax.exp)list) (binds:env) : env = 
    match e_list,id_list with 
      | e::e_rest,id::id_rest -> let v = (eval binds e) in 
                                    let rest = apply_function id_rest e_rest binds in 
                                        (id,v)::rest 
      | e::e_rest,[] -> failwith("Too many arguments")
      | [],id::id_rest -> failwith("Too few arguments")
      | [],[] -> []
       
                                   
and     

eval(binds :env)(e:HOF_syntax.exp) : value = 
   match e with 
	| HOF_syntax.Int n -> IntVal n 
        | HOF_syntax.Add (e1,e2) -> IntVal (to_int (eval binds e1) + to_int (eval binds e2))   
        | HOF_syntax.Sub (e1,e2) -> IntVal (to_int (eval binds e1) - to_int (eval binds e2))
        | HOF_syntax.Mul (e1,e2) -> IntVal (to_int (eval binds e1) * to_int (eval binds e2)) 
        | HOF_syntax.Let (x,e1,e2) ->let v1 =( eval binds e1 ) in 
				       let binds' = (x,v1)::binds in 
					 eval binds' e2  	
        | HOF_syntax.Id id -> lookup id binds 
        | HOF_syntax.If0 (e1,e2,e3) -> if (eval binds e1)=IntVal 0 then (eval binds e2) else (eval binds e3) 
        | HOF_syntax.Lambda (id_list,e3) -> Funv (id_list,e3,binds) 
        | HOF_syntax.Apply (e1 ,e_list) -> (match eval binds e1 with
					     |Funv (id_list,body,fn_env)->
                                              let bindsenv = apply_function (id_list) (e_list) (binds) in 
                                              eval (bindsenv@fn_env) body
					     | _ -> failwith ("expected function"))
        | HOF_syntax.Record (rec_list) -> RecordV rec_list 
        | HOF_syntax.GetField(e1,f) -> ( match eval binds e1 with 
  					    | RecordV rec_list -> (lookup_records f rec_list ) 
					    | _ -> failwith("Did not return record  type"))
        
	| HOF_syntax.SetField(e1,f,e2) -> ( match eval binds e1 with 
  					    | RecordV rec_list -> RecordV (set_field f rec_list e2 )
					    | _ -> failwith("Record not found"))  


(* Desugaring to create a derived language using the base language types *) 

module S= HOF_sugar 

let rec desugar_list (e_list : (S.exp) list) : (HOF_syntax.exp)list =
     match e_list with 
        | e::rest -> desugar e:: desugar_list rest
        | [] -> []  

and desugar_record (rec_list : (S.field*S.exp) list): (HOF_syntax.field*HOF_syntax.exp) list = 
     match rec_list with 
        | (f,e)::rest -> (f,(desugar e))::(desugar_record rest)
        | [] -> []        

and create_list (index:int) (e1:S.exp) (e2:S.exp) : ( HOF_syntax.field*HOF_syntax.exp ) list = 
        match e2 with  
          | S.Cons(e3,rest) -> (string_of_int index,desugar e1)::(create_list (index+1) e3 rest ) 
          | S.Empty -> [(string_of_int index,desugar e1)]
          | _ -> failwith("Cannot be anything but Cons or Empty") 

  
and  get_tail (e1:S.exp) (e2:S.exp) :HOF_syntax.exp =  
     match e2 with 
          |S.Empty -> desugar e1 
          |S.Cons(e3,e4) -> get_tail e3 e4
          | _ -> failwith("Cannot be anything but Cons or Empty")  

and  desugar (sugared_exp :S.exp) :HOF_syntax.exp =
     match sugared_exp with 
      | S.Id x -> HOF_syntax.Id x
      | S.Int n-> HOF_syntax.Int n 
      | S.Add(e1,e2) -> HOF_syntax.Add(desugar e1,desugar e2)
      | S.Sub(e1,e2) -> HOF_syntax.Sub(desugar e1,desugar e2) 
      | S.Mul(e1,e2) -> HOF_syntax.Mul(desugar e1,desugar e2) 
      | S.Let(x,e1,e2) -> HOF_syntax.Let(x,desugar e1,desugar e2)
      | S.If0(e1,e2,e3) -> HOF_syntax.If0(desugar e1,desugar e2,desugar e3)
      | S.Lambda(id_list,e3) -> HOF_syntax.Lambda(id_list,desugar e3) 
      | S.Apply(e1,e_list) -> HOF_syntax.Apply(desugar e1, desugar_list e_list) 
      | S.Record(rec_list) ->HOF_syntax.Record(desugar_record rec_list) 
      | S.GetField(e1,f) -> HOF_syntax.GetField(desugar e1,f) 
      | S.SetField(e1,f,e2) -> HOF_syntax.SetField(desugar e1,f,desugar e2) 
      | S.True -> HOF_syntax.Int 1 
      | S.False -> HOF_syntax.Int 0 
      | S.If(e1,e2,e3)-> HOF_syntax.If0(desugar e1,desugar e3,desugar e2) 
      | S.And(e1,e2) -> desugar (S.If0 (e1,S.Int 0,e2)) 
      | S.Or(e1,e2) -> desugar (S.If0 (e1,e2,S.Int 1))
      | S.IntEq(e1,e2) -> desugar (S.If0((S.Sub(e1,e2)),S.Int 1,S.Int 0))              
      | S.Empty -> HOF_syntax.Record([]); 
      | S.Cons(e1,e2) -> HOF_syntax.Record(create_list 0 e1 e2)
      | S.Head(e1) -> (match e1 with 
 			|S.Cons(head,rest)-> desugar head 
                        |_ -> failwith("list is empty")) 
      | S.Tail(e1) -> (match e1 with 
                        |S.Empty-> failwith("There is no tail") 
                        |S.Cons(e2,e3)->  get_tail e2 e3 
                        | _ -> failwith("illegal input"))   
      | S.IsEmpty(e1) -> match e1 with 
                        | S.Empty -> desugar S.True 
                        | _ -> desugar S.False  


(* Sugaring the base language just to make use of hte HOF_utils *) 

let rec sugar_list (e_list : (HOF_syntax.exp) list) : (S.exp)list =
     match e_list with
        | e::rest -> sugar e:: sugar_list rest
        | _ -> failwith("list expected")

and sugar_record (rec_list : (HOF_syntax.field*HOF_syntax.exp) list): (S.field*S.exp) list =
     match rec_list with
        | (f,e)::rest -> (f,(sugar e))::(sugar_record rest)
        | [] -> []

and  sugar (exp : HOF_syntax.exp) : S.exp  = 
       match exp with 
      | HOF_syntax.Id x -> S.Id x
      | HOF_syntax.Int n-> S.Int n
      | HOF_syntax.Add(e1,e2) -> S.Add(sugar e1,sugar e2)
      | HOF_syntax.Sub(e1,e2) -> S.Sub(sugar e1,sugar e2)
      | HOF_syntax.Mul(e1,e2) -> S.Mul(sugar e1,sugar e2)
      | HOF_syntax.Let(x,e1,e2) -> S.Let(x,sugar e1,sugar e2)
      | HOF_syntax.If0(e1,e2,e3) -> S.If0(sugar e1,sugar e2,sugar e3)
      | HOF_syntax.Lambda(id_list,e3) -> S.Lambda(id_list,sugar e3)
      | HOF_syntax.Apply(e1,e_list) -> S.Apply(sugar e1, sugar_list e_list)
      | HOF_syntax.Record(rec_list) ->S.Record(sugar_record rec_list)
      | HOF_syntax.GetField(e1,f) -> S.GetField(sugar e1,f)
      | HOF_syntax.SetField(e1,f,e2) -> S.SetField(sugar e1,f,sugar e2)

let read_eval (str : string) = match HOF_util.parse str with
  | HOF_util.Exp exp -> eval [] (desugar exp)
  | HOF_util.ParseError msg -> failwith ("parse error: " ^ msg)

(*string conversion functions *)

let rec  string_of_record (value:(HOF_syntax.field*HOF_syntax.exp) list) : string =
        match value with
           | (f,e)::rest ->  "element at index " ^ f ^ " has value " ^ string_of_int (to_int(eval [] e)) ^ "\n" ^  string_of_record rest
           | [] -> ""

let rec string_of_id(ids:(HOF_syntax.id list)) :string =
      match ids with 
          | id::rest -> id ^ " " ^ (string_of_id rest)
          | [] -> ""

(* Sugars the expression just to make use of the string_of_exp utility in the HOF_utils *) 
let rec string_of_body(exp :HOF_syntax.exp) : string = 
     HOF_util.string_of_exp ( sugar exp ) 

let rec string_of_env(e:env):string =  
            match e with 
             | (x,v)::rest -> x ^ " has value " ^string_of_value ( v ) ^ "\n" ^ string_of_env(rest) 
             | [] -> "" 

and string_of_value (v:value) : string = 
   match v with 
    | IntVal n -> string_of_int n 
    | Funv (id_list,body,env) ->"Variables : " ^ string_of_id(id_list) ^ "\n body :\n " ^ string_of_body(body) ^ " \n Environment "  ^ string_of_env(env)   
    | RecordV (value) -> string_of_record (value)   

 

let rec repl () = 
  print_string "> ";
  match HOF_util.parse (read_line ()) with
  | HOF_util.Exp exp -> 
    let v = eval []  (desugar exp) in
    print_string (string_of_value v);
    print_newline ();
    repl ()
  | HOF_util.ParseError msg ->
    print_string msg;
    print_newline ();
    repl ()

let _ =  
  match Array.to_list Sys.argv with
  | [ exe; "repl" ] -> print_string "Press Ctrl + C to quit.\n"; repl ()
  | _ -> () 

(* Tests *)

TEST "fail test" = 
eval [] (HOF_syntax.Add(HOF_syntax.Int 2,HOF_syntax.Int 3)) = IntVal 5 
TEST "add test" = 
eval [] (HOF_syntax.Add(HOF_syntax.Int 2,HOF_syntax.Int 3)) = IntVal 5
TEST "sub test" = 
eval [] (HOF_syntax.Sub(HOF_syntax.Int 2,HOF_syntax.Int 3)) = IntVal (-1) 
TEST "mul test" = 
eval [] (HOF_syntax.Mul(HOF_syntax.Int 2,HOF_syntax.Int 3)) = IntVal 6
TEST "let test" = 

eval [] (HOF_syntax.Let("x",HOF_syntax.Int 2,HOF_syntax.Add(HOF_syntax.Id "x",HOF_syntax.Int 3))) = IntVal 5
TEST " If test" = 
eval [] (HOF_syntax.If0(HOF_syntax.Int 0,HOF_syntax.Int 2,HOF_syntax.Int 4)) = IntVal 2
TEST " If test" =
eval [] (HOF_syntax.If0(HOF_syntax.Int 3,HOF_syntax.Int 2,HOF_syntax.Int 5)) = IntVal 5


TEST = 
let id = ["x"] in 
 let f= (HOF_syntax.Lambda(id,HOF_syntax.Add(HOF_syntax.Id "x",HOF_syntax.Int 2))) in 
   eval[] (HOF_syntax.Apply(f,[HOF_syntax.Int 2;])) = IntVal 4  
TEST " function " = 
let id_list = ["x";"y"] in 
eval [] (HOF_syntax.Lambda(id_list,HOF_syntax.Add(HOF_syntax.Id "x",HOF_syntax.Id "y"))) = Funv(id_list,HOF_syntax.Add(HOF_syntax.Id "x",HOF_syntax.Id "y"),[])
TEST " apply function " = 
let id =["x";"y"] in  
  let f = HOF_syntax.Lambda(id,HOF_syntax.Add(HOF_syntax.Id "x",HOF_syntax.Id "y")) in 
    eval[] (HOF_syntax.Apply (f , [HOF_syntax.Int 1;HOF_syntax.Int 3;]) ) = IntVal 4

TEST "record test" = 
eval[] (HOF_syntax.Record [("x",HOF_syntax.Int 50);("y",HOF_syntax.Int 40);]) = RecordV [("x",HOF_syntax.Int 50);("y",HOF_syntax.Int 40)] 
TEST "record getfield" = 
let rec_list = (HOF_syntax.Record [("x",HOF_syntax.Int 50);("y",HOF_syntax.Int 40);]) in 
  eval [] (HOF_syntax.GetField(rec_list,"x")) = IntVal 50
TEST "record setfield" = 
let rec_list = (HOF_syntax.Record [("x",HOF_syntax.Int 50);("y",HOF_syntax.Int 40);]) in 
  eval [] (HOF_syntax.SetField(rec_list,"x",HOF_syntax.Int 99)) = RecordV [("x",HOF_syntax.Int 99);("y",HOF_syntax.Int 40)]

TEST "list creation"= 
let lst = S.Cons(S.Int 1,S.Cons(S.Int 2,S.Empty)) in 
desugar lst = HOF_syntax.Record([("0",HOF_syntax.Int 1);("1",HOF_syntax.Int 2);])
TEST "list head" = 
let lst = S.Cons(S.Int 1,S.Cons(S.Int 2,S.Empty)) in
desugar (S.Head (lst)) = HOF_syntax.Int 1  
TEST "list tail" = 
let lst = S.Cons(S.Int 1,S.Cons(S.Int 2,S.Empty)) in 
desugar (S.Tail (lst)) = HOF_syntax.Int 2
TEST "list isEmpty" = 
let lst = S.Empty in 
desugar (S.IsEmpty(lst)) = HOF_syntax.Int 1 
TEST " IntEq " = 
let exp = S.IntEq(S.Mul(S.Int 2, S.Int 3),S.Int 6) in
eval [] ( desugar (exp)) = IntVal 1 
let exp = S.And(S.Or(S.False,S.True),S.Int 5) in 
eval [] ( desugar ( S.If(exp,S.True,S.False))) = IntVal 1  

(* Tests using the parser *)
TEST "Arithmetic tests " = 
let exp = "(2+3)*(3+4)-(2+3)+4" in 
read_eval exp = IntVal 34
TEST "logical operators tests " = 
let exp = "(2+3) && ((3==3) || false ) " in 
read_eval exp = IntVal 1 
TEST "let " = 
let exp = " let x = 12 in let y = 15 in let z = 8 in x+y+z " in 
read_eval exp = IntVal 35
TEST "if else"= 
let exp = " let x = 10 in if x == 10 then x*10 else x*100 " in 
read_eval exp = IntVal 100 
TEST "if 0" = 
let exp = " let x = 0 in if0 x then 1 else 2 " in 
read_eval exp = IntVal 1 
TEST "records " = 
let exp = "{a:1,b:10,c:(5*6)}.c" in
read_eval exp = IntVal 30  
TEST "lists " = 
let exp = " head(1::2::empty) " in
read_eval exp = IntVal 1 
TEST "head" = 
let exp = " empty?(empty) " in 
read_eval exp = IntVal 1 
TEST "tail " = 
let exp = " tail(1::(3+4+6)::empty) " in 
read_eval exp = IntVal 13 
TEST "empty list " = 
  try 
   let _ = read_eval " 
     head (empty ) " in 
   false 
   with Failure "list is empty" -> true 
TEST "cannot recur" = 
  try 
    let _ = read_eval "
      let fac = lambda (n) . if0 n then 1 else n * fac(n - 1) in
      fac(5)" in
    false (* expected exception! *)
  with Failure "Free string " -> true
TEST "self application" =
  read_eval "
    let make_fac = lambda(self, n).
                      if0 n then 1 else n * self(self, n - 1) in
    let fac = lambda (n) . make_fac(make_fac, n) in 
    fac(5)"
  = IntVal 120
