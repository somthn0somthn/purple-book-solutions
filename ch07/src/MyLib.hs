module MyLib where

-- all 4 equiv
-- same

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive = \x -> \y -> if x > y then y + 5 else x + 5

mflip :: (a -> a -> b) -> a -> a -> b
mflip f x y = f y x

--Exercise: Variety pack
k:: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4-1), 10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

--see above
--see above, no
--k1, k3

f :: (a, b, c) -> (d, e, f) -> ((a,d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))


--Exercises: Case practice

functionC :: Ord a => a -> a -> a
functionC x y =
    case x > y of
        True -> x
        False -> y
    
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = 
    case even n of
        True -> n + 2
        False -> n

nums :: (Ord a, Num a) => a -> a
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

--Exercises: Artful dodgy

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

--1
--11
--22
--21
--12

--11
--21
--21
--22
--31
--23

--Exercises: Gaurd duty

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
   | y >= 0.7 = 'C'
   | y >= 0.9 = 'A'
   | y >= 0.8 = 'B'
   | y >= 0.59 = 'D'
   | y < 0.59 = 'F'
   where y = x / 100

--all cases fall to first line
--Type checks, but doesnt work as intended. Returns 'C'

--b
--lists
pal :: Eq a => [a] -> Bool
pal xs 
   | xs == reverse xs = True
   | otherwise        = False

--c
--Num a

numbers :: (Ord a, Num a) => a -> a
numbers x 
   | x < 0 = -1
   | x == 0 = 0
   | x > 0  = 1

--Chapter Exercises
--MC
--d
--b
--a
--b
--a

--Lets write code

tensDigit :: Integral a => a -> a 
tensDigit x = d
    where xLast = x `div` 10
          d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
   where (xLast, last) = x `divMod` 10
         d     = xLast `mod` 10

--yes, same type
hunsD :: Integral a => a -> a 
hunsD x = d2
   where (xLast2, last2) = x `divMod` 100
         d2             = xLast2 `mod` 10 

foldBool :: a -> a -> Bool -> a
foldBool x y b = 
    case b of
        True -> x
        False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b 
   | b == True = x
   | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

--module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read a, Show b, Read b) => a -> b
roundTrip'' a = read shown 
   where
    shown :: String
    shown = show a

main:: IO()
main = do
    print (roundTrip'' 4::Int)
    print (id 4)