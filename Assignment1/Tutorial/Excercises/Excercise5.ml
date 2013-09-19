type time = Time of int*int*int 

let time1 : time = Time(1,30,00)  

let seconds_since_midnight2 (t :time) : int = 
 match t with 
  | Time ( hours , mins , secs ) -> secs + 60 * ( mins + 60 * hours ) 

let example_function ( t:time) : time = 
 match t with 
  Time ( hours , mins , secs ) -> Time ( hours , mins , secs)  

TEST "example" = seconds_since_midnight2 time1 = 3600 + 30*60  
  
let time2 : time = Time(1,30,00) 

TEST "example2 " = example_function time1 = time2 
