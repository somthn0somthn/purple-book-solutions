{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MyLib where
import Data.Int (Int8)
import Data.Char
import Data.Maybe (catMaybes)
import Data.Map (Map, fromListWith)


-- Dog types

-- type constructor

-- * -> *

-- Num a => Doggies a
-- Doggies Integer
-- Doggies String
-- both
-- doge -> DogueDeBordeaux doge
-- DogueDeBordeaux String

-- Vehicles

data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini 
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline = 
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

type Size = Int

data Vehicle = Car Manufacturer Price
  | Plane Airline Size
    deriving (Eq, Show)

--Vehicle

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man

--it will throw an error if passed a plane

--arity :: the number of arguments a function or constructor takes

--Cardinality

--1
--3
--65356
--Int has a huge range, and Integer is not bounded so cardinality is infinite
--8 bits => 2 ^ 8 = 256

--For Example

data Example = MakeExample deriving (Eq, Show)

--Example, no information
--yes, Eq & Show

data MyExample = MakeMyExample Int deriving (Eq, Show)

--it now accepts a value argument, e.g. its a unary data constructor

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42



--Logic goats


instance TooMany (Int, String) where
    tooMany (int, str) = str > "cat" && int > 42

instance TooMany (Int, Int) where
    tooMany (x, y) = (x + y) > 42

instance TooMany Integer where
    tooMany n = n > 42

instance (Num a, TooMany a) => TooMany (a,a) where
    tooMany (x, y) = tooMany (x + y)

--Pity the Bool

data BigSmal = 
    Big Bool
  | Small Bool
  deriving (Eq, Show)

--cardinality 4 b/c Big & Small are identity function in terms of cardinality
--Big True/False + Small True/Fase = 4

data NumberOrBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

--cardinality = 256 + 2 = 258
--it will wrap with an overflow warning

--How does your garden grow?

type Gardener = String

data Garden =
    Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show

--Programmers

data OperatingSystem = 
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDSTill
  | Mac 
  | Windows
  deriving (Eq, Show)

data ProgLang = 
    Haskell
  | Agda
  | Idris
  | PureScript 
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDSTill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

--The Quad

data Quad =
    One
  | Two
  | Three 
  | Four 
  deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = Left One
--8 instances

prodQuad :: (Quad, Quad)  -- (,) -> Quad -> Quad
prodQuad = (One, One)
--16 instances

funcQuad :: Quad -> Quad 
funcQuad One = One
--16 instances

prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, True, True)
--8 instances

gTwo :: Bool -> Bool -> Bool
gTwo True True = True
--8 instances

fTwo :: Bool -> Quad -> Quad
fTwo True One = One
--65536 instances

--Write map for BinaryTree

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2 
       (Node Leaf 5 Leaf)


mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node a b c) = [b] ++ (preorder a) ++ (preorder c )

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node a b c) = (inorder a) ++ [b] ++ (inorder c)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node a b c) = (postorder a) ++ (postorder c) ++ [b] 

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b)
          -> b 
          -> BinaryTree a
          -> b
foldTree f b tree = foldr f b $ postorder tree

--Chapter exercises
--MC
--a
--c
--b
--c

--Ciphers

vigenere str key = matchSpacing str $ foldr (\(x, y) acc -> shift x y : acc) [] zipped
  where 

    zipped :: [(Char, Char)]
    zipped =  zip strFiltered wrapKeyword

    strFiltered :: String
    strFiltered = filter (not . isSpace) (map toUpper str)

    wrapKeyword :: String
    wrapKeyword = take (length strFiltered) $ f (map toUpper key)

    f :: String -> String
    f str = str ++ f str

    shift :: Char -> Char -> Char
    shift x y = chr $ modLetter (ord x + (ord y - 65))

    modLetter :: Int -> Int
    modLetter x
      | x > 90 = (x - 90) + 64
      | otherwise = x
    

--As-patterns

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf first@(x:xs) (a:as) = if x == a then isSubseqOf xs as else
    isSubseqOf first as

capitalizeWords :: String -> [(String, String)]
capitalizeWords str = map (\word@(x:xs) -> (word, (toUpper x):xs)) theWords
  where
    theWords = words str 

--Language exercises

capitalizeWord :: String -> String
capitalizeWord word@(x:xs) = (toUpper x) : xs

capitalizeParagraph :: String -> String
capitalizeParagraph str = matchSpacing str $ concat $ capitalizeParagraph' (capitalizeLead $ words str)

--some helpers

matchSpacing :: String -> String -> String
matchSpacing [] _ = []
matchSpacing _ [] = []
matchSpacing (r:rs) junk@(j:js)
    | isSpace r = ' ' : matchSpacing rs junk
    | otherwise = j : matchSpacing rs js

capitalizeParagraph' :: [String] -> [String]
capitalizeParagraph' [] = []
capitalizeParagraph' (x:xs)
  | '.' `elem` x = x : capitalizeParagraph' (capitalizeLead xs)
  | otherwise    = x : capitalizeParagraph' xs

capitalizeLead :: [String] -> [String]
capitalizeLead (x:xs) = capitalizeWord x : xs

--Phone exercise

type Digit = Char

data DaPhone = DaPhone [(Digit, String)]

phone :: DaPhone
phone = DaPhone [
    ('1', " ")
  , ('2', "ABC")
  , ('3', "DEF")
  , ('4', "GHI")
  , ('5', "JKL")
  , ('6', "MNO")
  , ('7', "PQRS")
  , ('8', "TUV")
  , ('9', "WXYZ")
  , ('*', "^")
  , ('0', "+_")
  , ('#', ".,")
  ]

pressCount :: DaPhone -> Char -> Maybe (Digit, Int) --(Digit, Int)
pressCount (DaPhone phoneList) ch =
    findPressCount phoneList (toUpper ch)
      where
        findPressCount [] _ = Nothing
        findPressCount ((digit, letters):xs) ch
          | isDigit ch = Just (ch, 1)
          | ch `elem` letters = Just (digit, count ch letters)
          | otherwise = findPressCount xs ch
        
        count :: Char -> String -> Int
        count ch str = go ch str 1
          where 
            go ch (x:xs) index
              | ch == x = index
              | otherwise = 
                  go ch xs (index + 1)

isUppercase :: Char -> Maybe (Digit, Int)
isUppercase ch
  | isUpper ch = Just ('*', 1)
  | otherwise = Nothing

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol OK. Have u ever tasted alcohol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "OK. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

mapConvo :: String -> [Maybe (Digit, Int)]
mapConvo []  = [Nothing]
mapConvo (x:xs)
  | isUpper x = Just ('*', 1) : mapConvo ((toLower x):xs) 
  | otherwise = pressCount phone x : mapConvo xs

fingerTaps :: String -> Int
fingerTaps str = calculate $ catMaybes (mapConvo str)
  where 
    calculate xs = foldr (\x acc -> snd x + acc) 0 xs

--the remainder seem like hashmap questison and not relevant so skipping

--Hutton's Razor

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b

printExpr :: Expr -> String
printExpr (Lit x)   = show x
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b