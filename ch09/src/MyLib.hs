module MyLib where

import Data.Bool (bool)
import Data.Char
import Ciphers

myHead :: [a] -> Maybe a
myHead (x:_) = Just x
myHead []    = Nothing

--EnumFromTo

{- eftBool :: Bool -> Bool -> [Bool]
eftBool x y = if x > y then [] else evaluate x y
  where 
    evaluate :: Bool -> Bool -> [Bool]
    evaluate x y
      | x == y = [x]
      | otherwise  = [x, y]  -}

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x < y = x : eftBool (succ x) y
  | x == y = [x]
  | otherwise  = []


eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
   | x < y = x : eftOrd (succ x) y
   | x == y  = [x]
   | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x < y = x : eftInt (succ x) y
  | x == y = [x]
  | otherwise  = []

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x < y = x : eftChar (succ x) y
  | x == y = [x]
  | otherwise  = []

--Thy fearful symmetry

myWords :: String  ->  [String]
myWords x
  | x == ""  = []
  | x == " " = []
  | otherwise = word : myWords (drop (length word + 1) x)
    where
      word = takeWhile (/=' ') $ dropWhile (== ' ') x

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
myLines :: String -> [String]
myLines x
  | x == ""  = []
  | x == " " = []
  | otherwise = line : myLines (drop (length line + 1) x)
    where
        line = takeWhile (/= '\n') $ dropWhile (== '\n') x

myBreak :: Char -> String -> [String]
myBreak c x
  | x == ""  = []
  | x == " " = []
  | otherwise = break : myBreak c (drop (length break + 1) x)
    where
        break = takeWhile (/= c) $ dropWhile (== c) x

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"



main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
           == shouldEqual)

--Comprehend thy lists

--mySqr = [x^2 | x <- [1..10]]
--[1,4,9,16,25,36,49,64,81,100]

--[4,16,36,64,100]

--[(1,64), (1,81), (1,100), (4,64), (4,81), (4,100) .. (49,64), (49,81), (49,100)]

--[(1,64), (1,81), (1,100), (4,64), (4,81)]

--Square cube

mySqr = [x ^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

comp1 :: Num a => [a] -> [a] -> [(a,a)]
comp1 as bs = [(x, y) | x <- as, y <- bs]

comp2 :: (Num a, Ord a) => [a] -> [a] -> [(a,a)]
comp2 as bs = [(x, y) | x <- as, x < 50, y <- bs, y < 50]

lastComp = length $ comp2 mySqr myCube

--Bottom madness
--will it blow up?

--yes
--no
--yes
--no
--yes
--no
--yes
--no
--no
--yes

--whnf, nf
--whnf
--neither
--neither
--neither
--neither
--whnf

--More bottoms
-- ⊥
-- value
-- ⊥
-- takes a list of chars/string and maps T/F for whether the char is a vowel

--[1, 4, 9 .. 100]
--[1,10,20]
--[15,15,15]

mapBool :: (Num a, Eq a) => [a] -> [a]
mapBool = map (\x -> bool (x) (-x) (x == 3))

--Filtering

filt3 :: Integral a => [a] -> [a]
filt3 = filter (\x -> (x `rem` 3) == 0) 

lengFilt3 :: Integral a => [a] -> Int
lengFilt3 = length . filt3

filtWords :: [String] -> String -> [String]
filtWords words str = filter (\x -> x `notElem` words) (myWords str)

--Zipping exercises

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _  _     []     = []
zipWith' _ []     _      = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

--Chapter exercises
--isUpper :: Char -> Bool
--toUpper :: Char -> Char

filterHello :: String -> String
filterHello str = filter isUpper str

firstUpper :: String -> String
firstUpper (s:ss) = toUpper s : ss

firstUpperRec :: String -> String
firstUpperRec "" = ""
firstUpperRec (s:ss) = toUpper s : firstUpperRec ss

headUpper :: String -> Char
headUpper = toUpper . head

--Ciphers
--cc Ciphers.hs

--Writing you own standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = True `elem` (map f xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem a (x:xs) = (a == x) || myElem a xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x xs = any (==x) xs

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> x)

myMaximumBy :: (a -> a -> Ordering)
          -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = if f x (myMaximumBy f xs) == GT then x else myMaximumBy f xs 

myMinimumBy :: (a -> a -> Ordering)
          -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) = if f x (myMaximumBy f xs) == LT then x else myMaximumBy f xs 

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare