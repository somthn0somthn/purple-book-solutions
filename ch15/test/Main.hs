module Main (main) where

import MyLib
import Test.QuickCheck



main :: IO ()
main = do  
    putStrLn "Testing from test/Main.hs"
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    putStrLn "Ch exercises"
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdenAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    putStrLn "the readers"
    quickCheck combineAssoc
    quickCheck compAssoc
    quickCheck (semigroupAssoc :: ValidationAssoc)
    putStrLn "adding l/r identies for the monoid exercises"
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    putStrLn "the readers"
    quickCheck monoidCombineLeftIdentity
    quickCheck monoidCombineRightIdentity
    quickCheck monoidCompLeftIdentity
    quickCheck monoidCompRightIdentity
    putStrLn "runMem"
    quickCheck memAssoc
    quickCheck monoidMemLeftIdentity
    quickCheck monoidMemRightIdentity


