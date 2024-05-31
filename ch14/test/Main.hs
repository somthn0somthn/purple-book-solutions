module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List (intersperse, sort)
import Data.Char
import Test.Hspec.QuickCheck (prop)

digitToWord :: Int -> String
digitToWord n 
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"


digits :: Int -> [Int]
digits x = go x []
  where go x xs
         | x < 10 = x : xs
         | otherwise = 
              go (x `div` 10) ((x `mod` 10) : xs)

wordNumber :: Int -> String
wordNumber x = concat $ intersperse "-" xs
  where xs = map digitToWord $ digits x

{- main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
        
    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ do
            digits 100 `shouldBe` [1, 0 ,0]
    
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one" -}

half :: (Ord a, Fractional a) => a -> a
half x = x / 2

halfIdentity :: (Ord a, Fractional a) => a -> a
halfIdentity = (*2) . half

prop_half :: Double-> Bool
prop_half x = halfIdentity x == x 

----------------------------------

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
    snd $ foldr go (Nothing, True) xs
  where  
    go _ status@(Nothing, True) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _ ) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered = listOrdered . sort

---------------------------------

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y = x + y == y + x

----------------------------------

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y = x * y == y * x

---------------------------------

quotRem' :: Int -> Int -> Bool
quotRem' x y = (quot x y) * y + (rem x y) == x

divMod' :: Int -> Int -> Bool
divMod' x y = (div x y) * y + (mod x y) == x

genInt :: Gen Int
genInt = elements [1..100000]

prop_quotRem' :: Property
prop_quotRem' = forAll genInt $ \x -> forAll genInt $ \y -> quotRem' x y

prop_divMod' :: Property
prop_divMod' = forAll genInt $ \x -> forAll genInt $ \y -> divMod' x y

----------------------------------

expAssociative :: Int -> Int -> Int -> Bool
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
--not associative


expCommutative :: Int -> Int -> Bool
expCommutative x y = x ^ y == y ^ x
--not commutative

----------------------------------

genList :: (Arbitrary a, Eq a) => Gen [a]
genList = do
    a <- arbitrary
    return [a]

genListInt :: Gen [Int]
genListInt = genList

genListChar :: Gen [Char]
genListChar = genList

rev_prop :: Eq a => [a] -> Bool
rev_prop xs = (reverse . reverse) xs == id xs

prop_listInt :: Property
prop_listInt = forAll genListInt $ \xs -> rev_prop xs

prop_listChar:: Property
prop_listChar = forAll genListChar $ \xs -> rev_prop xs

----------------------------------

genChar :: Gen Char
genChar = elements ['a' .. 'z']

dollar_prop :: Eq a => (a -> a) -> a -> Bool
dollar_prop f x = (f $ x) == f x 

prop_dollarChar :: Property
prop_dollarChar = forAll genChar $ \x -> dollar_prop (toUpper) x

prop_dollarInt :: Property
prop_dollarInt = forAll genInt $ \x -> dollar_prop (+1) x

compose_prop :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
compose_prop f g x = (f . g) x ==  f (g x)

prop_composeInt :: Property 
prop_composeInt = forAll genInt $ \x -> compose_prop (+1) (*2) x

----------------------------------

foldCons_prop :: Eq a => [a] -> [a] -> Bool
foldCons_prop xs ys = foldr (:) xs ys == (++) xs ys

prop_foldCons :: Property
prop_foldCons = forAll genListChar $ 
                   \xs -> forAll genListChar $ 
                   \ys -> foldCons_prop xs ys

--this property does not hold


---------------------------------

genNestedInt :: Gen [[Int]]
genNestedInt = listOf genListInt

concat_prop :: Eq a => [[a]] -> Bool
concat_prop xxs = foldr (++) [] xxs == concat xxs

prop_concat :: Property
prop_concat = forAll genNestedInt $ \xxs -> concat_prop xxs

---------------------------------

lenTake_prop :: Int -> [a] -> Bool
lenTake_prop n xs = length (take n xs) == n


prop_lenTake :: Property
prop_lenTake = forAll genInt $ \n -> forAll genListInt $ \xs ->
                    lenTake_prop n xs
--does not hold

----------------------------------

readShow_prop :: (Read a, Show a, Eq a) => a -> Bool
readShow_prop x = (read (show x)) == x

genArb :: Arbitrary a => Gen a
genArb = do
    a <- arbitrary
    return a

genString :: Gen String
genString = genArb

prop_readShowInt :: Property
prop_readShowInt = forAll genInt $ \x -> readShow_prop x

prop_readShowString :: Property
prop_readShowString = forAll genString $ \x -> readShow_prop x

--Failure

--Essentially, sqrt requires a floating point number and because floating 
--points only have finite precision and don't understand things like recursive 
--digits (e.g 1/3 = 0.333333333333...) the sqrt can not be perfectly to applied 
--to numbers that aren't the result of squaring whole numbers. On the tail end,
--when that not-100% accurate floating point value is squared it returns
--a floating point with a minute, but noticeable, rounding error

--Idempotence

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x) : xs

prop_capitalize :: Property
prop_capitalize = forAll genString $ \x -> 
                  (capitalizeWord x == twice capitalizeWord x)
                  && (capitalizeWord x == fourTimes capitalizeWord x)

prop_sortIdempotence :: Property
prop_sortIdempotence = forAll genListInt $ \x -> 
                         (sort x == twice sort x)
                         && (sort x == fourTimes sort x)

--Make a Gen random generator for the datatype

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

{- instance Arbitrary Fool where
    arbitrary = oneof [return Fulse,
                       return Frue] -}

instance Arbitrary Fool where
    arbitrary = frequency [(2, return Fulse),
                           (1, return Frue)]

genFool :: Gen Fool
genFool = arbitrary

---------------------------------
--quickCheck Cesear - alpha only
genAlphaString :: Gen String
genAlphaString = listOf $ elements ['a'..'z'] 

cesearShift :: Int -> Char -> Char
cesearShift x c = chr $ modLetterInt letterInt
  where
    letterInt     = (x `mod` 26) + ord c

    modLetterInt x 
      | x > 122 = (x `mod` 122 ) + 96
      | otherwise = x

appCesearCyph :: Int -> String -> String
appCesearCyph _ "" = ""
appCesearCyph x str = map (cesearShift x . toLower) $ filter (\x -> x `elem` alpha) str
  where
    alpha :: [Char]
    alpha = ['a'..'z'] ++ ['A'..'Z'] 

cesear_prop :: Int -> String -> Bool
cesear_prop x str = (appCesearCyph (-x) $ appCesearCyph x str) == str

prop_cesear :: Property
prop_cesear = forAll genInt $ \x -> forAll genAlphaString $ \s ->
                cesear_prop x s
----------------------------------
--CONT FROM:: quick check vigenere

genAlphaUpperString :: Gen String
genAlphaUpperString = listOf $ elements ['A'..'Z'] 

vigenere :: [Char] -> [Char] -> String
vigenere "" _    = ""
vigenere str ""  = str
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
    
    matchSpacing :: String -> String -> String
    matchSpacing [] _ = []
    matchSpacing _ [] = []
    matchSpacing (r:rs) junk@(j:js)
      | isSpace r = ' ' : matchSpacing rs junk
      | otherwise = j : matchSpacing rs js

unVigenere :: [Char] -> [Char] -> String
unVigenere "" _    = ""
unVigenere str ""   = str
unVigenere str key = matchSpacing str $ foldr (\(x, y) acc -> shift x y : acc) [] zipped
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
    shift x y = chr $ modLetter (ord x - (ord y - 65))

    modLetter :: Int -> Int
    modLetter x
      | x > 90 = (x - 90) + 64
      | x < 65 = (x + 90) - 64
      | otherwise = x
    
    matchSpacing :: String -> String -> String
    matchSpacing [] _ = []
    matchSpacing _ [] = []
    matchSpacing (r:rs) junk@(j:js)
      | isSpace r = ' ' : matchSpacing rs junk
      | otherwise = j : matchSpacing rs js

vigenere_prop :: String -> String -> Bool
vigenere_prop str key = (unVigenere (vigenere str key) key) == str

prop_vigenere :: Property
prop_vigenere = forAll genAlphaUpperString $ \x -> forAll genAlphaUpperString $ \y -> vigenere_prop x y
----------------------------------
main :: IO ()
main = do
    putStrLn "testing from ch14/test/Main.hs" 
    quickCheck prop_half
    quickCheck prop_listOrdered
    quickCheck plusAssociative
    quickCheck plusCommutative
    quickCheck multAssociative
    quickCheck multCommutative
    quickCheck prop_quotRem'
    quickCheck prop_divMod'
    --quickCheck expAssociative
    --quickCheck expCommutative
    quickCheck prop_listInt
    quickCheck prop_listChar
    quickCheck prop_dollarChar
    quickCheck prop_dollarInt
    quickCheck prop_composeInt
    --quickCheck prop_foldCons
    quickCheck prop_concat
    --quickCheck prop_lenTake
    quickCheck prop_readShowInt
    quickCheck prop_readShowString  
    quickCheck prop_capitalize
    quickCheck prop_sortIdempotence
    quickCheck prop_cesear
    quickCheck prop_vigenere

