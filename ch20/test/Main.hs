module Main where

import MyLib
import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

constantTrigger :: Constant String (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
constantTrigger = undefined

twoTrigger :: Two String (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
twoTrigger = undefined

threeTrigger :: Three String String (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
threeTrigger = undefined

threePrimeTrigger :: Three' String (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
threePrimeTrigger = undefined

listTrigger :: List (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
listTrigger = undefined

main :: IO ()
main = do 
    putStrLn "Constant type"
    quickBatch (foldable constantTrigger)    
    
    putStrLn "Two type"
    quickBatch (foldable twoTrigger)
    
    putStrLn "Three type"
    quickBatch (foldable threeTrigger)

    putStrLn "Three' type"
    quickBatch (foldable threePrimeTrigger)

    putStrLn "List type"
    quickBatch (foldable threePrimeTrigger)
