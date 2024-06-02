module MyLib where

import Data.Semigroup 
import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) Nada  Nada        = Nada
    (<>) Nada (Only a)     = Only a
    (<>) (Only a) Nada     = Only a
    (<>) (Only a) (Only b) = Only $ a <> b

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend = (<>)

--Madness

type Adjective = String
type Adverb     = String
type Noun       = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat
    [
        e,
        "! he said ",
        adv,
        " as he jumped into his car ",
        noun,
        " and drove off with his ",
        adj,
        " wife."
    ]

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m  -> Bool
monoidLeftIdentity m = (mempty <> m)  == m

monoidRightIdentity :: (Eq m, Monoid m)
                   => m  -> Bool
monoidRightIdentity m = (m <> mempty) == m

type S = String
type B = Bool
type MA =   S -> S -> S -> B

--Maybe another Monoid

newtype First' a = 
    First' { getFirst' :: Optional a}
    deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) (First' (Only a)) (First' (Only b)) = First' (Only a)
    (<>)  _                (First' (Only a)) = First' (Only a)
    (<>) (First' (Only a)) _                 = First' (Only a)
    (<>) _                 _                 = First' Nada


instance Monoid (First' a) where
    mempty = First' Nada
    {- mappend = (<>) -}

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ First' Nada, 
                       return $ First' (Only a) ]

firstMappend :: First' a 
             -> First' a     
             -> First' a     
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
    First' String -> Bool

--Chapter Exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c = a <> (b <> c) == (a <> b) <> c

type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

------

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance (Monoid a) =>  Monoid (Identity a) where
    mappend = (<>)
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)
    
type IdenAssoc =
    Identity String -> Identity String -> Identity String -> Bool 

-----

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mappend = (<>)
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoAssoc =
    Two String String -> Two String String -> Two String String -> Bool


--------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

type ThreeAssoc =
    Three String String String 
 -> Three String String String 
 -> Three String String String 
 -> Bool

--------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) 
             => Semigroup (Four a b c d) where
    (<>) (Four a b c d) (Four a' b' c' d') 
          = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) 
          => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (Four a b c d)

type FourAssoc =
    Four String String String String
 -> Four String String String String
 -> Four String String String String 
 -> Bool

-------

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (<>) (BoolConj False) _        = BoolConj False
    (<>) _  (BoolConj False)       = BoolConj False
    (<>) _ _                       = BoolConj True

instance Monoid BoolConj where
    mappend = (<>)
    mempty = BoolConj True

instance Arbitrary BoolConj where
    arbitrary = oneof [return (BoolConj True),
                       return (BoolConj False)] 

type BoolConjAssoc = 
    BoolConj -> BoolConj -> BoolConj -> Bool

-------

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (<>) (BoolDisj True) _        = BoolDisj True
    (<>) _  (BoolDisj True)       = BoolDisj True
    (<>) _ _                      = BoolDisj False

instance Monoid BoolDisj where
    mappend = (<>)
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = oneof [return (BoolDisj True),
                       return (BoolDisj False)] 

type BoolDisjAssoc = 
    BoolDisj -> BoolDisj -> BoolDisj -> Bool

----------

data Or a b =
    Fst a 
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
    (<>) (Snd a) _        = (Snd a)
    (<>) (Fst a) (Snd b)  = (Snd b)
    (<>) (Fst a) (Fst b)  = (Fst b)

instance (Arbitrary a, Arbitrary b) 
        => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return (Fst a),
               return (Snd b)]

type OrAssoc =
    Or String String -> Or String String -> Or String String -> Bool

------

newtype Combine a b =
    Combine { unCombine :: (a -> b)}

instance Show (Combine a b) where
    show f = "Combine <function>"

instance (Semigroup b) => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g)  = Combine $ \x -> f x <> g x

instance (Monoid b) => Monoid (Combine a b) where
    mappend = (<>)
    mempty = Combine mempty

instance (Arbitrary a, CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        func <- arbitrary 
        return $ Combine func

combineAssoc :: Int -> Combine Int String -> Combine Int String -> Combine Int String -> Bool
combineAssoc x a b c = unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x


monoidCombineLeftIdentity :: Int -> Combine Int String  -> Bool
monoidCombineLeftIdentity x a = unCombine (mempty <> a) x  == unCombine a x

monoidCombineRightIdentity :: Int -> Combine Int String  -> Bool
monoidCombineRightIdentity x a = unCombine (a <> mempty) x  == unCombine a x

-----

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show f = "Comp <function>"

instance (Semigroup a) => Semigroup (Comp a) where
    (<>) (Comp a) (Comp b) = Comp $ a <> b

instance (Monoid a) => Monoid (Comp a) where
    mappend = (<>)
    mempty = Comp mempty

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        func <- arbitrary
        return $ Comp func
    
compAssoc :: String -> Comp String -> Comp String -> Comp String -> Bool
compAssoc s a b c = unComp (a <> (b <> c)) s == unComp ((a <> b) <> c) s

monoidCompLeftIdentity :: String -> Comp String  -> Bool
monoidCompLeftIdentity x a = unComp (mempty <> a) x  == unComp a x

monoidCompRightIdentity :: String -> Comp String  -> Bool
monoidCompRightIdentity x a = unComp (a <> mempty) x  == unComp a x
----

data Validation a b = 
    Failure' a | Success' b
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
    (<>) (Success' a) _ = Success' a
    (<>) _ (Success' b) = Success' b
    (<>) (Failure' a) (Failure' b) = Failure' $ a <> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return (Failure' a),
               return (Success' b)]

type ValidationAssoc =
    Validation String String -> Validation String String -> Validation String String -> Bool

------

newtype Mem s a = 
    Mem {
        runMem :: s -> (a, s)
    }

instance Show (Mem s a) where
    show (Mem _) = "Mem <function>"

instance (Arbitrary a, CoArbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Mem s a) where
    arbitrary = do
        func <- arbitrary
        return $ Mem func

instance Semigroup a => Semigroup (Mem s a) where
    (<>) (Mem f) (Mem g) = Mem $ \s ->
                              let
                                (a, s') = f s
                                (a', s'') = g s'
                              in 
                                ((a <> a'), s'')

instance Monoid a => Monoid (Mem s a) where
    mappend = (<>)
    mempty = Mem $ \s -> (mempty, s)

memAssoc :: String 
          -> Mem String String 
          -> Mem String String 
          -> Mem String String 
          -> Bool
memAssoc s a b c = runMem (a <> (b <> c)) s == runMem ((a <> b) <> c) s

monoidMemLeftIdentity :: String -> Mem String String -> Bool
monoidMemLeftIdentity s a = runMem (mempty <> a) s == runMem a s

monoidMemRightIdentity :: String -> Mem String String -> Bool
monoidMemRightIdentity s a = runMem (a <> mempty) s == runMem a s

