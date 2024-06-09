module MyLib where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Identity a =
    Identity a
    deriving (Eq, Show)

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

--Library functions

sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

elem' :: (Foldable t, Eq a)
     => a -> t a -> Bool
elem' a = foldr (\x acc -> (x == a) || acc) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' ta
  | length ta <= 1 = Nothing
  | otherwise = Just $ minimum ta

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' ta
  | length ta <= 1 = Nothing
  | otherwise = Just $ maximum ta

null' ::  (Foldable t) => t a -> Bool
null' ta = (length ta) == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x : acc) []

fold' :: (Foldable t, Monoid m) 
      => t m -> m
fold' = foldMap (<> mempty) 

foldMap' :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty

--Chapter exercises

data Constant a b =
    Constant b 
    deriving (Eq, Show)

instance Foldable (Constant a) where
    foldr f z (Constant x) = f x z
    foldl f z (Constant x) = f z x
    foldMap f (Constant x) = f x

instance (Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq b) => EqProp (Constant a b) where
    (=-=) = eq

----------

data Two a b = 
    Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldr f z (Two a b) = f b z
    foldl f z (Two a b) = f z b
    foldMap f (Two a b) = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-------

data Three a b c =
    Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z
    foldl f z (Three a b c) = f z c 
    foldMap f (Three a b c) = f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq 

-------

data Three' a b = 
    Three' a b b
    deriving (Eq, Show)

instance Foldable (Three' a) where
    foldr f z (Three' a b b') = f b (f b' z)
    foldMap f (Three' a b b') = f b <> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

---------

data Four' a b =
    Four' a b b b
    deriving (Eq, Show)

instance Foldable (Four' a) where
    foldr f z (Four' a b c d) = f b $ f c $ f d z
    foldMap f (Four' a b c d) = f b <> f c <> f d 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

--------

--NOTE: added List just for testing filterF in repl

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

instance Foldable List where
    foldr f z Nil         = z
    foldr f z (Cons a ls) = f a (foldr f z ls)
    foldMap f Nil         = mempty
    foldMap f (Cons a ls) = f a <> (foldMap f ls)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = oneof [ return Nil,
                        Cons <$> arbitrary <*> arbitrary ]

instance Eq a
    => EqProp (List a) where
  (=-=) = eq


--------

filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a)
           , Show (f a))
        => (a -> Bool) -> t a -> f a
filterF g = foldMap (\x -> if g x then pure x else mempty)