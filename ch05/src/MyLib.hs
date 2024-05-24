{-# LANGUAGE NoMonomorphismRestriction #-}

module MyLib  where
--c
--d
--b
--a
--e

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer
                -> Bool
                -> Integer
curriedFunction i b = 
    i + (nonsense b)

uncurriedFunction :: (Integer, Bool)
                  -> Integer
uncurriedFunction (i, b) =
    i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = 
    \i -> \b -> i + (nonsense b)

--Char -> Char -> Char
--Char
--Num b => b
--Double
--[Char]
--Eq b => b -> [Char]
--(Num a, Ord a) => a
--(Num a, Ord a) => a
--Integer

attempt :: a -> a
attempt a = undefined

tripleA :: a -> a -> a
tripleA x y = x

tripleA' :: a -> a -> a
tripleA' x y = y

tripleA'' :: Num a => a -> a -> a
tripleA'' x y = x + y

dropA :: a -> b -> b
dropA x y = y

f x y = x + y + 3


--Apply yourself
--(++) :: [Char] -> [Char] -> [Char]
--(*) :: Fractional a => a -> a -> a
--take :: Int -> [Char] -> [Char]
--(>) :: Int -> Int -> Bool
--(<) :: Char -> Char -> Bool

--Chapter Exercises
--c
--a
--b
--a

--Determine the type
--Num a => a
--Num a => (a, [Char])
--(Integer, [Char])
--Bool
--Int
--Bool

--Num a => a

--Num a => a > a

--Fractional a => a

--[Char]

--Does it compile
--no, bigNum is fully applied => bigNum = (^) 5
--yes
--no, c use b as a func => c = a 10
--no, c not defined => b = 10000

--Type variable or specific type constructor
--fully polymorphic, concrete, crete
--fully polymorphic, constrained, concrete
--fully polymorphic, fully polymorphic, concrete

--Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a  -> Bool
functionC x y =
    if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y 

--Given a type, write the function
myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, zet)
    where zet = yToZ . xToY $ x

i :: a -> a
i = id

c :: a -> b -> a
c x y = x

--yes, c and c'' are the same

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r xs = xs

co :: (b -> c) -> (a -> b) -> a -> c
co g f = g . f 

a :: (a -> c) -> a -> a
a g a = a

a' :: (a -> b) -> a -> b
a' f = f

--Fix it

--module sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: [Char]
sing = if (x > y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

sing' :: [Char]
sing' = if (x < y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"

--module Arith3Broken where

main :: IO ()
main = do
    print $ 1 + 2
    putStrLn "10"
    print (negate (-1))
    print ((+) 0 blah)
       where blah = negate 1

--Type-Kwon-Do
--1
f' :: Int -> String
f' = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f'

--2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

--3
data X
data Y
data Z

xz :: X -> Z 
xz = undefined 

yz :: Y -> Z 
yz = undefined 

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (z', z'')
    where z'  = xz x 
          z'' = yz y 

--4
munge :: (x -> y)
      -> (y -> (w, z))
      -> x 
      -> w
munge f g x = fst . g $ (f x)

