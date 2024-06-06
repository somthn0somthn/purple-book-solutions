
{-# LANGUAGE FlexibleContexts #-}

module MyLib where

import Data.List (elemIndex)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import System.Posix.Internals (lstat)
import Test.QuickCheck (Arbitrary (arbitrary))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup)
import Control.Applicative (liftA3)

--Lookups
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3][4..6])

y :: Maybe Integer
y = lookup 3 $ zip [1..3][4..6]

z :: Maybe Integer 
z = lookup 2 $ zip [1..3][4..6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs = [1..3]
ys = [4..6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''


--Identity instance

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a) 

--Constant instance

newtype Constant a b = 
    Constant { getConstant :: a}
    deriving (Eq, Ord, Show)

instance Functor (Constant a)  where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure a = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant (a <> b)

--Fixer uppper

one = const <$> Just "Hello" <*> pure "World"

two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

--17.8 ZipList Monoid

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) Nil Nil                   = Nil
    (<>) Nil ls                    = ls
    (<>) ls Nil                    = ls
    (<>) (Cons a ls) ls'  = Cons a (ls <> ls')

instance Monoid (List a) where
    mappend = (<>)
    mempty  = Nil

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _  = Nil
    (<*>) _  Nil = Nil
    (<*>) (Cons f ls) ls' = (fmap f ls') <> (ls <*> ls')

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [ return Nil,
                        Cons <$> arbitrary <*> arbitrary ]

instance Eq a
    => EqProp (List a) where
  (=-=) = eq

-------

newtype ZipList' a =
    ZipList' [a]
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take 3000 l 
          ys' = let (ZipList' l) = ys
                in take 3000 l

instance Semigroup a => Semigroup (ZipList' a) where
    (<>) (ZipList' xs) (ZipList' ys) 
      = ZipList' $ combine xs ys
      where 
        combine _ []          = []
        combine [] _          = []
        combine (x:xs) (y:ys) = (x <> y) : combine xs ys

instance Monoid a => Monoid (ZipList' a) where
    mempty = ZipList' []
    mappend = (<>)

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure a = ZipList' (repeat a)
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ combine fs xs
     where 
        combine _     []     = []
        combine []     _     = []
        combine (f:fs)(x:xs) = (f x) : combine fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

--------

data Validation e a =
    Failure' e
  | Success' a 
  deriving (Eq, Show)

instance (Semigroup e, Semigroup a) 
  => Semigroup (Validation e a) where
 (<>) (Failure' e) (Success' a)   = Failure' e
 (<>) (Success' a) (Failure' e)   = Failure' e
 (<>) (Failure' e) (Failure' e')  = Failure' (e <> e')
 (<>) (Success' a) (Success' a')   = Success' (a <> a')

instance (Monoid a, Monoid e) => Monoid (Validation e a) where
    mempty = Success' mempty
    mappend = (<>)

instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' (f a)

instance (Monoid e) => 
         Applicative (Validation e) where
  pure a = Success' a
  (<*>) (Failure' f) (Success' a) = (Failure' f)
  (<*>) (Success' f) (Failure' e)  = (Failure' e)
  (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')
  (<*>) (Success' f) (Success' a)   = Success' (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        oneof [ return (Failure' e),
                return (Success' a) ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

--Chapter exercises
--1 pure :: a -> [a]
--  (<*>) :: [(a -> b)] -> [a] -> [b]

--2 pure :: a -> IO a
-- IO (a -> b) -> IO a -> IO b

--3 pure :: a -> (t, a)
-- (<*>) :: (t, (a -> b)) -> (t, a) -> (t, b)

--4 pure :: a -> (e -> a)
--  (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

data Pair a = Pair a a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Pair a) where
    (<>) (Pair a b) (Pair a' b') = Pair (a <> a') (b <> b')

instance (Monoid a) => Monoid (Pair a) where
    mempty = Pair mempty mempty
    mappend = (<>)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
     pure a  = Pair a a
     (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)
    
instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

-------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure a = Two mempty a
    (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = mappend

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure a = Three mempty mempty a
    (<*>) (Three a b f) (Three a' b' c) = Three (a <> a')(b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-------

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
    (<>) (Three' a b c) (Three' a' b' c') = Three' (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
    mempty = Three' mempty mempty mempty 
    mappend = (<>)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure a = Three' mempty a a 
    (<*>) (Three' a f g) (Three' a' b c) = Three' (a <> a') (f b) (g c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

---------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
           Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four a' b' c' d') 
          = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
         Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty
    mappend = (<>)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure a = Four mempty mempty mempty a
    (<*>) (Four a b c f) (Four a' b' c' d) 
          = Four (a <> a')(b <> b')(c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

----------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
    (<>) (Four' a b c d) (Four' a' b' c' d') 
        = Four' (a <> a') (b <> b') (c <> c') (d <> d')
    
instance (Monoid a, Monoid b) => Monoid (Four' a b) where
    mempty = Four' mempty mempty mempty mempty
    mappend = (<>)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
    pure a = Four' mempty mempty mempty a
    (<*>) (Four' a b c f) (Four' a' b' c' d) 
        = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

-----------

--Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
