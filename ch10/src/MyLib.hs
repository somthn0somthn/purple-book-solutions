module MyLib where

import Data.Time
import Data.Data (Data)

-- Understanding folds
-- a,b,c

{- (1 * 1) : 2 : 3 : []
(2 * 1) : 3 : []
(3 * 2) : []
6 : []
6 -}

-- c
-- a

-- foldr (++) [] ["woot", "WOOT", "woot"]
-- foldr max [] ["fear is the little death"]
-- foldr (&&) True [False, True]
-- foldr (&&) False [False, True]
-- foldl (flip  ((++) . show)) "" [1..5]
-- foldr const 1 [1..5]
-- foldr const 'a' "tacos"
-- foldl (flip const) 'a' "burritos"
-- foldl (flip const) 1 [1..5]

-- ** REMEMBER** foldl is inappropriate for lists that are or

-- could potentially be infinit because it forces recursion
-- across the entire list

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, World!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      )
  ]

filterDate :: [DatabaseItem] -> [DatabaseItem]
filterDate db = filter func db
  where 
    func (DbDate _) = True
    func         _  = False

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map getNum $ (filter func db)
  where
    func (DbNumber _) = True
    func _            = False

    getNum :: DatabaseItem -> Integer    
    getNum (DbNumber num) = num

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum . map getTime $ filter func db
  where
    func (DbDate _) = True
    func _          = False

    getTime (DbDate utc) = utc

sumDb :: [DatabaseItem] -> Integer
sumDb db = sum $ map getNum $ (filter func db)
  where
    func (DbNumber _) = True
    func _            = False

    getNum :: DatabaseItem -> Integer    
    getNum (DbNumber num) = num

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum numbers) / fromIntegral (length numbers)
  where
    func (DbNumber _) = True
    func _            = False

    getNum :: DatabaseItem -> Integer    
    getNum (DbNumber num) = num

    numbers = map getNum $ filter func db

fibs = 1 : scanl (+) 1 fibs

fibsN x = fibs !! x

--Scans exercises

fibs20 = take 20 fibs

fibsLT100 = takeWhile (<100) fibs

--Chapter exercises

stops= "pbtdkg"
vowels = "aeiou"

make3Tups :: String -> String -> [(Char, Char, Char)]
make3Tups cons vwls = [(x, y, z) | x <- cons, y <- vwls, z <- cons]

make3Tups' :: String -> String -> [(Char, Char, Char)]
make3Tups' cons vwls = filter ps $ [(x, y, z) | x <- cons, y <- vwls, z <- cons]
  where
    ps ('p', _, _) = True
    ps _           = False

nouns :: [String]
nouns = ["dog", "cat", "book", "car", "house", "tree", "computer", "phone", "apple", "river"]

verbs :: [String]
verbs = ["run", "jump", "swim", "read", "write", "sing", "dance", "eat", "sleep", "laugh"]

mk3Words :: [String] -> [String] -> [String]
mk3Words nouns verbs = [x ++ " " ++ y ++ " " ++ z | x <-nouns, y <- verbs, z <-nouns]

--seekritFunc takes a String sentence, and figures out the average length
--of a word in that sentence

fracSeekritFunc :: Fractional a => String -> a
fracSeekritFunc x = fromIntegral (sum (map length (words x))) /
                         fromIntegral (length (words x))

--Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False 

myElemFold :: (Eq a) => a -> [a] -> Bool
myElemFold x = foldr ((||) . (==x)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny a = any (==a)

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> [x]) $ concat xs

myMaximum :: (a -> a -> Ordering) -> [a] -> a
myMaximum f xs = foldr (\x acc -> if f x acc == LT then acc else x) (head xs) xs

myMinimum :: (a -> a -> Ordering) -> [a] -> a
myMinimum f xs = foldr (\acc x -> if f acc x == LT then acc else x) (head xs) xs