type time= Time of int*int*int  

let seconds_since_midnight2 (t :time) : int = 
 match t with 
  | Time ( hours , mins , secs ) -> secs + 60 * ( mins + 60 * hours ) 


let time_diff (t1:time) (t2:time) : int = 
   seconds_since_midnight2 t1 - seconds_since_midnight2 t2 

TEST = let t = Time(1,1,1) in time_diff t t = 0 
TEST = time_diff(Time(23,59,59)) (Time(0,0,0)) = 86399   
