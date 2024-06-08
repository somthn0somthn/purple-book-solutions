module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import MyLib

triggerSum :: Sum String (Int, Int, Int)
triggerSum = undefined

triggerNope :: Nope (Int, Int, Int)
triggerNope = undefined

triggerBahEither :: BahEither String (String, String, String)
triggerBahEither = undefined

triggerIdentity :: Identity (Int, Int, Int)
triggerIdentity = undefined

triggerList :: List (String, String, String)
triggerList = undefined

main :: IO ()
main = do
    putStrLn "Sum >> Either type"
    quickBatch (monad triggerSum)
    putStrLn "Nope type"
    quickBatch (monad triggerNope)
    putStrLn "BahEither type"
    quickBatch (monad triggerBahEither)
    putStrLn "Identity type"
    quickBatch (monad triggerIdentity)
    putStrLn "List type"
    quickBatch (monad triggerList)