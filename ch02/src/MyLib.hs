module MyLib (someFunc) where

import Learn
import Mult

someFunc :: IO ()
someFunc = putStrLn "someFunc"

commonPat :: (Num a) => a -> a -> a
commonPat x y = (x + y) * 3

triple x = x * 3

area x = 3.14 * (x * x)

double x = x * 2

--x = 7
--y = 10

--f = x + y

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

waxOff x = triple x