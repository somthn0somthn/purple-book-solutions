module MyLib where

import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "Jello!"

someFunction :: Int -> Int
someFunction x = x + 1

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
            go (n - d) d (count + 1)

integral :: (Integral a) => a -> a -> a
integral num  mult = go num mult 0
   where go n  m tot 
          | m == 0 = tot
          | otherwise =
               go n (m - 1) (tot + n)

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
          => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [(1, return Nothing)
              , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater