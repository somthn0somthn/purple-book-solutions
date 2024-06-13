module Main (main) where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import MyLib
import Test.QuickCheck (quickCheck)

main :: IO ()
main = do
    putStrLn "Moi Monoid tests"
    quickCheck monoidMoiAssoc
    quickCheck monoidMoiLeftIdentity
    quickCheck monoidMoiRightIdentity

    putStrLn "Moi Functor tests"
    quickCheck functorMoiId
    quickCheck functorMoiComposition

    putStrLn "Moi Applicative tests"
    quickCheck applicativeMoiId
    quickCheck applicativeMoiComposition
    quickCheck applicativeMoiHomomorphism
    quickCheck applicativeMoiInterchange

    putStrLn "Moi Monad tests"
    quickCheck monadMoiRightIdentity
    quickCheck monadMoiLeftIdentity
    quickCheck monadMoiAssoc