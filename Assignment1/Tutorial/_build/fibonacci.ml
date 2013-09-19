let rec fibonacci ( n:int ) : int = 
  if n = 1 then  
    0 
  else if n = 2 then 
    1 
  else 
    fibonacci(n-1) + fibonacci(n-2) 


TEST "example" = 
  fibonacci 7 = 1     
