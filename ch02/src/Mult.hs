module Mult (mult1, mult2, mult3, mult4) where

mult1 = x * y
  where x = 5
        y = 6

mult2 = x * 3 + y
   where x = 3
         y = 1000

mult3 = x * 5
   where y = 10
         x = 10 * 5 + y

mult4 = z / x + y
   where x = 7
         y = negate x
         z = y * 10