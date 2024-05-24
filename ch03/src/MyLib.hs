module MyLib (someFunc) where

import Reverse

someFunc :: IO ()
someFunc = putStrLn "someFunc"

topLevelFunction :: Integer -> Integer
topLevelFunction x = 
    x + woot + topLevelValue
    where woot :: Integer
          woot = 10

topLevelValue :: Integer
topLevelValue = 5

{- main :: IO ()
main = do
    putStrLn "Count for me"
    putStr "one, two"
    putStr ", three,& "
    putStrLn "four" -}

{- printSecond :: IO ()
printSecond = do
    putStrLn greeting 
 -}
a :: String -> String
a x = x ++ "!"

b :: String -> String
b x = (x !! 4) : ""

c :: String -> String
c = drop 9

thirdChar :: String -> Char
thirdChar x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs str = z ++ " " ++ y ++ " " ++ x 
  where
    x = take 5 str 
    y = take 2 $ drop 6 str
    z = drop 9 str

{- main :: IO ()
main = do
    putStrLn greeting
    printSecond
greeting = "Yarrrrr" -}