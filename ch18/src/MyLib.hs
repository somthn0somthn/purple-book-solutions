module MyLib where

import Control.Monad (join, ap, liftM, liftM2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


--The answer is the exercise
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f 

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

--Either Monad

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Sum a b) where
    (<>) (First a) (First a')   = First (a <> a')
    (<>) _          (First a)   = First a
    (<>) (First a)  _           = First a
    (<>) (Second b) (Second b') = Second (b <> b')

instance (Monoid a, Monoid b) => Monoid (Sum a b) where
    mempty = Second mempty
    mappend = (<>)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) (First a) _           = First a
    (<*>) _ (First a)           = First a
    (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (>>=) (First a)   _ = First a
    (>>=) (Second b)  f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [ return (First a),
                return (Second b) ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

--Chapter Exercises

data Nope a = 
    NopeDotJpg
    deriving (Eq, Show)

instance Semigroup (Nope a) where
    (<>) _ _ = NopeDotJpg

instance Monoid (Nope a) where
    mempty = NopeDotJpg
    mappend = (<>)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure a = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

-----------

data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show) 

instance (Semigroup a, Semigroup b) => Semigroup (BahEither b a) where
    (<>) (PRight b) (PRight b') = PRight (b <> b')
    (<>) (PRight b) _         = (PRight b)
    (<>) _ (PRight b)         = (PRight b)
    (<>) (PLeft a) (PLeft a') = PLeft (a <> a')
    

instance (Monoid a, Monoid b) => Monoid (BahEither b a) where
    mempty = PLeft mempty
    mappend = (<>)

instance Functor (BahEither b) where
    fmap _ (PRight b) = PRight b
    fmap f (PLeft a)  = PLeft (f a)

{- instance (Monoid b) => Applicative (BahEither b) where
    pure a = PLeft a
    PLeft f <*> PLeft a = PLeft (f a)
    PRight b <*> _ = PRight b
    _ <*> PRight b = PRight b -}

instance (Monoid b) => Applicative (BahEither b) where
    pure a = PLeft a
    (<*>) (PLeft f) (PLeft a) = PLeft (f a)
    (<*>) (PRight b) _        = PRight b
    (<*>) _ (PRight b)        = PRight b
   

instance (Monoid b) => Monad (BahEither b) where
    return = pure 
    (>>=) (PRight b) _ = PRight b
    (>>=) (PLeft a)  f = f a

instance (Arbitrary a, Arbitrary b) 
    => Arbitrary (BahEither b a) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            elements [(PRight b), (PLeft a)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
    (=-=) = eq

--------

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    (>>=) (Identity a) f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

--------

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) Nil ls = ls
    (<>) ls Nil = ls
    (<>) (Cons a as) ls = Cons a (as <> ls)

instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a ls) = Cons (f a) (fmap f ls)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _   = Nil
    (<*>) _ Nil   = Nil
    (<*>) (Cons f fs) lst = (fmap f lst) <> (fs <*> lst)

instance Monad List where
    return = pure
    (>>=) Nil _ = Nil
    (>>=) (Cons a as) f = f a <> (as >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary =
        oneof [return Nil, (Cons <$> arbitrary <*> arbitrary)]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

--------

j :: Monad m => m (m a) -> m a
j mma = mma >>= (\x -> x >>= return)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= \a -> mb >>= \b -> return (f a b)
 
a :: Monad m =>  m a -> m (a -> b) -> m b
a ma mf= mf >>= \f -> ma >>= \a -> return (f a)

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (f x) >>= (\x -> ((:) x) <$> meh xs f) 

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
