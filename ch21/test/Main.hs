module Main where

import MyLib
import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

identityTrigger :: Identity (Maybe Int, Maybe Int, Int, [Int])
identityTrigger = undefined

constantTrigger :: Constant (Maybe Int, Maybe Int, Int, [Int]) String
constantTrigger = undefined

optionalTrigger :: Optional (Maybe Int, Maybe Int, Int, [Int])
optionalTrigger = undefined

listTrigger :: List (Maybe Int, Maybe Int, Int, [Int])
listTrigger = undefined

threeTrigger :: Three String String (Maybe Int, Maybe Int, Int, [Int])
threeTrigger = undefined

pairTrigger :: Pair String (Maybe Int, Maybe Int, Int, [Int])
pairTrigger = undefined

bigTrigger :: Big String (Maybe Int, Maybe Int, Int, [Int])
bigTrigger = undefined

biggerTrigger :: Bigger String (Maybe Int, Maybe Int, Int, [Int])
biggerTrigger = undefined

sTrigger :: S [] (Maybe Int, Maybe Int, Int, [Int])
sTrigger = undefined

treeTrigger :: Tree (Maybe Int, Maybe Int, Int, [Int])
treeTrigger = undefined

main :: IO ()
main = do
    putStrLn "Identity type"
    quickBatch (traversable identityTrigger)

    putStrLn "Constant type"
    quickBatch (traversable identityTrigger)

    putStrLn "Optional type"
    quickBatch (traversable optionalTrigger)

    putStrLn "List type"
    quickBatch (traversable listTrigger)

    putStrLn "Three type"
    quickBatch (traversable listTrigger)

    putStrLn "Pair type"
    quickBatch (traversable pairTrigger)

    putStrLn "Big type"
    quickBatch (traversable bigTrigger)

    putStrLn "Bigger type"
    quickBatch (traversable biggerTrigger)

    putStrLn "S type"
    quickBatch (traversable sTrigger)

    putStrLn "Tree type"
    quickBatch (traversable treeTrigger)