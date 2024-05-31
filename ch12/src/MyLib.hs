module MyLib where
import Data.List --(intersperse)
import Data.Char
--Chapter exercises

-- *
-- * , * -> *

replaceThe :: String -> String
replaceThe str = concat $ intersperse " " $ map f (words str)
  where 
    notThe :: String -> Maybe String
    notThe str
      | str == "the" = Nothing
      | otherwise = Just str
    
    f :: String -> String
    f str = case notThe str of
        Nothing  -> "a"
        Just str -> str

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go theWords 0
  where
    theWords = words (map toLower str)

    go (x:xs) count
      | x == "the" && length xs /= 0 = go xs (count + checkHead xs)
      | xs == [] = count
      | otherwise = go xs count
    
    checkHead :: [String] -> Integer
    checkHead (x:xs) = if (head x) `elem` "aeiou" then 1 else 0

countVowels :: String -> Integer
countVowels str = foldr (\x acc -> if x `elem` "aeiouAEIOU" then acc + 1 else acc) 0 str

--Validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"
consonants = filter (`notElem` vowels) ['a'..'z']  

mkWord :: String -> Maybe Word'
mkWord str
  | (count str vowels) > (count str consonants) = Nothing
  | otherwise = Just $ Word' str

  where
    count :: String -> String -> Int
    count str letters = foldr (\x acc -> if x `elem` letters then acc + 1 else acc) 0 str

--It's only natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0    = Nothing
  | int == 0   = Just Zero
  | otherwise  = Just (Succ (decrement $ int - 1))
  where
    decrement :: Integer -> Nat
    decrement 0 = Zero
    decrement x = Succ (decrement $ x - 1) 
  
--Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _)   = True
isJust (Nothing)  = False

isNothing :: Maybe a -> Bool
isNothing (Just _)  = False
isNothing (Nothing) = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f (Nothing) = z
mayybee _ f (Just x)  = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z (Nothing) = z
fromMaybe _ (Just x)  = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToLIst (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []       = []
catMaybes (Nothing:xs)  = catMaybes xs
catMaybes ((Just x):xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if length catted == length xs then Just catted else Nothing
  where
    catted = catMaybes xs

lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\x acc -> take x ++ acc) [] xs
  where
    take (Left a)  = [a]
    take (Right _) = []

rights' :: [Either a b] -> [b]
rights' xs = foldr (\x acc -> take x ++ acc) [] xs
  where
    take (Left _)  = []
    take (Right a) = [a]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right a) = Just $ f a
eitherMaybe' _ (Left _)  = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Right b) = Just $ f b
eitherMaybe'' _ (Left _)  = Nothing

--Unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Just (a, c) -> a : myUnfoldr f c
  Nothing     -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr g a
  where 
    g a = Just (a, f a)

--Finally something other than a list!

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
  Just (a, b, c) -> Node (unfold f a) b (unfold f c)
  Nothing        -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild x = take x (unfold f 0)
  where 
    take :: Integer -> BinaryTree Integer -> BinaryTree Integer
    take n _ | n <= 0 = Leaf
    take _ Leaf       = Leaf
    take n (Node a b c) = Node (take (n - 1) a) b (take (n - 1) c)
  
    f a = Just (a + 1, a, a + 1)