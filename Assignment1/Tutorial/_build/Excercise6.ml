type time = Time of int*int*int 

let seconds_to_time (x:int) : time = 
  let s = x mod 60 in 
  let x = (x-s)/60 in 
  let m = x mod 60 in 
  let x = (x-m)/60 in 
  let h = x in 
  Time(h,m,s) 

TEST = seconds_to_time 0 = Time(0,0,0); 

