
type time = Time of int*int*int 

let seconds_since_midnight2 (t :time) : int =
 match t with
  | Time ( hours , mins , secs ) -> secs + 60 * ( mins + 60 * hours )

let seconds_to_time (x:int) : time =
  let s = x mod 60 in
  let x = (x-s)/60 in
  let m = x mod 60 in
  let x = (x-m)/60 in
  let h = x in
  Time(h,m,s)

let tick (t:time) : time = 
   seconds_to_time ( (seconds_since_midnight2 t) + 1)   

TEST = tick (Time (0,0,59)) = Time (0,1,0)  
TEST = tick (Time (1,59,59)) = Time (2,0,0) 
TEST = tick (Time (23,59,59)) = Time (24,0,0)
