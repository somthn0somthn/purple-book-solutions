module MyLib where

import Data.List (intersperse)

--Intermission: Exercise

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b 
applyTimes n f b = applyTimes (n - 1) f (f b)

{- applyTimes 5 (+1) 5
applyTimes 4 (+1) 6
applyTimes 3 (+1) 7
applyTimes 2 (+1) 8
applyTimes 1 (+1) 9
applyTimes 0 (+1) 10 = 10
 -}

--Chapter Exercises
--[[Bool]]  
--b
--d
--b

--Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--"woops  mrow woohoo!"

--"1 mrow haha"

--"woops mrow 2 mrow haha"

--"woops mrow blue mrow haha"

--"pink mrow haha mrow green mrow woops mrow blue"

--"are mrow Pugs mrow awesome"

--Recursion

{- 15 divided by 2 ==
15 - 2, 13(subtracted 1 time)
   - 2, 11(subtracted 2 times)
   - 2, 9(subtracted 3 times)
   - 2, 7(subtracted 4 times) 
   - 2, 5(subtracted 5 times)
   - 2, 3(subtracted 6 times)
   - 2, 1(subtracted 7 times) -}

recursive :: (Num a, Eq a) => a -> a
recursive 0 = 0
recursive n = n + recursive (n -1)


integral :: (Integral a) => a -> a -> a
integral num  mult = go num mult 0
   where go n  m tot 
          | m == 0 = tot
          | otherwise =
               go n (m - 1) (tot + n)

--Fixing dividedBy

data DividedResult = 
    Result Integer
  | DividedByZero
  deriving (Show)

dividedBy :: Integral a => a -> a -> (DividedResult, a)
dividedBy _ 0      = (DividedByZero, 0)
dividedBy num denom  = (Result result, remainder)
   where 
    result :: Integer
    result = fromIntegral $ num `div` denom

    remainder = num `rem` denom

--McCarthy 91 function

mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n <= 100 = 91
  | otherwise = n - 10

--Numbers into words

digitToWord :: Int -> String
digitToWord n 
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"


digits :: Int -> [Int]
digits x = go x []
  where go x xs
         | x < 10 = x : xs
         | otherwise = 
              go (x `div` 10) ((x `mod` 10) : xs)

wordNumber :: Int -> String
wordNumber x = concat $ intersperse "-" xs
  where xs = map digitToWord $ digits x

