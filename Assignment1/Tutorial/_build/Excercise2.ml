let seconds_since_midnight ( h:int) (m:int) (s:int) : int = 
  s + 60*(m + 60* h )  

TEST "example1" = 
   seconds_since_midnight 1 30 40  = 3600 + 30*60 + 40 
