let leap_year y =
  if y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0) then true
  else false
              
