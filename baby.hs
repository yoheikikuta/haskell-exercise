doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

fizzbuzz x = if x `mod` 15 == 0 then "fizzbuzz" 
  else if x `mod` 5 == 0 then "buzz" 
  else if x `mod` 3 == 0 then "fizz" 
  else show x
