module Main where

import MyLib
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.String (String)

triggerList :: List (Int, Int, Int)
triggerList = undefined

triggerZipList :: ZipList' (Int, Int, Int)
triggerZipList = undefined

triggerValidation :: Validation String (Int, Int, Int)
triggerValidation = undefined

triggerPair :: Pair (Int, Int, Int)
triggerPair = undefined

triggerTwo :: Two String (Int, Int, Int)
triggerTwo = undefined

triggerThree :: Three String String (Int, Int, Int)
triggerThree = undefined

triggerThree' :: Three' String (Int, Int, Int)
triggerThree' = undefined

triggerFour :: Four String String String (Int, Int, Int)
triggerFour = undefined

triggerFour' :: Four' String (Int, Int, Int)
triggerFour' = undefined

main :: IO ()
main = do
    putStrLn "List type"
    quickBatch (applicative triggerList)
    putStrLn "ZipList type"
    quickBatch (applicative triggerZipList)
    putStrLn "Validation type"
    quickBatch (applicative triggerValidation)
    putStrLn "Pair type"
    quickBatch (applicative triggerPair)
    putStrLn "Two type"
    quickBatch (applicative triggerTwo)
    putStrLn "Three type"
    quickBatch (applicative triggerThree)
    putStrLn "Three' type"
    quickBatch (applicative triggerThree')
    putStrLn "Four type"
    quickBatch (applicative triggerFour)
    putStrLn "Four' type"
    quickBatch (applicative triggerFour')




--11:55\