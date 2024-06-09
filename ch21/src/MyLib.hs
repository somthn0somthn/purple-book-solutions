module MyLib where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



--Chapter exercises

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f z (Identity a) = f a z
    foldMap f (Identity a) = f a

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

-------

newtype Constant a b = 
    Constant { getConstant :: a}
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ z _ = z
    foldMap f _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure (Constant a)
             
instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
    (=-=) = eq

--------

data Optional a =
    Nada 
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep a) = f a z

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = 
        oneof [ return Nada,
                Yep <$> arbitrary]

instance (Eq a) => EqProp (Optional a) where
    (=-=) = eq

---------

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a ls) = Cons (f a) (fmap f ls) 

instance Foldable List where
    foldr _ z Nil         = z
    foldr f z (Cons a ls) = f a (foldr f z ls)

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a ls) = Cons <$> f a <*> (traverse f ls)

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = 
        oneof [ return Nil,
                Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

-----------

data Three a b c = 
    Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f z (Three a b c) = f c z
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) 
    => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-------------

data Pair a b = 
    Pair a b
    deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldr f z (Pair a b) = f b z
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = (Pair a) <$> (f b)

instance (Arbitrary a, Arbitrary b)  => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

-----------

data Big a b =
    Big a b b 
    deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldr f z (Big a b b') = f b (f b' z)
    foldMap f (Big a b b') = (f b) <> (f b')

instance Traversable (Big a) where
    traverse f (Big a b b') = (Big a) <$> (f b) <*> (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq
----------------

data Bigger a b = 
    Bigger a b b b
    deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
    foldr f z (Bigger a b c d) = f b $ f c $ f d z
    foldMap f (Bigger a b c d) = (f b) <> (f c) <> (f d)

instance Traversable (Bigger a) where
    traverse f (Bigger a b c d) = (Bigger a) <$> (f b) <*> (f c) <*> (f d) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq


-------------

--CONT from S

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance (Foldable n) => Foldable (S n) where
    foldr f z (S na a) = f a z
    foldMap f (S na a) = (foldMap f na) <> (f a)

instance (Traversable n) => Traversable (S n) where
    traverse f (S na a) = S <$> (traverse f na) <*> (f a)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
    arbitrary =
        S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
         => EqProp (S n a) where
    (=-=) = eq

-----------

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Leaf a)       = Leaf (f a)
    fmap f (Node ta a tb) = Node (fmap f ta) (f a) (fmap f tb)

instance Foldable Tree where
    foldr _ z Empty            = z
    foldr f z (Leaf a)         = f a z 
    foldr f z  (Node ta a tb)  = foldr f z $ treeToList (Node ta a tb)

    foldMap f Empty            = mempty
    foldMap f (Leaf a)         = f a
    foldMap f (Node ta a tb)   = (foldMap f ta) <> (f a) <> (foldMap f tb) 

treeToList :: Tree a -> [a]
treeToList Empty = []
treeToList (Leaf a) = [a]
treeToList (Node ta a tb) = (treeToList ta) ++ [a] ++ (treeToList tb)

instance Traversable Tree where
    traverse _ Empty          = pure Empty
    traverse f (Leaf a)       = Leaf <$> (f a) -- :: f Tree a
    traverse f (Node ta a tb) = Node <$> (traverse f ta) <*> (f a) <*> (traverse f tb) 

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary =
        oneof [return Empty, 
               Leaf <$> arbitrary,
               Node <$> arbitrary <*> arbitrary <*> arbitrary]
            
instance (Eq a) => EqProp (Tree a) where
    (=-=) = eq