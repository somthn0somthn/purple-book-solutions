{-# LANGUAGE FlexibleInstances #-}

module MyLib where

import GHC.Arr


--Be kind

-- *
-- * -> *
-- * -> * -> *

--Heavy lifting

a = (+1) <$> (read "[1]" :: [Int])

b = (fmap . fmap) (++ "lol")  (Just ["Hi,", "Hello"])

c = (*2) <$> (\x -> x - 2)

d = ((return '1' ++) . show) <$>
    (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . (("123"++) . show) <$> ioi
    in (*3) <$> changed

--Instances of Func

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

data Pair a  = Pair a a
   deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b
    deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

data Four a b c d = Four a b c d 
    deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
    deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

--no its a nullary type constructor, or fully applied of kind *

data Trivial = Trivial
    deriving (Eq, Show)

--wont compile
{- instance Functor Trivial where
    fmap f (Trivial) = Trivial
 -}

--Possibly

data Possibly a = 
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope     = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

--Short exercise

data Sum a b = 
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a)  = First a
    fmap f (Second b) = Second (f b)

--2. to do so it would have to apply to Either, which is not unary, but
--rather is of kind * -> * -> *

--Chapter exercises

--1. no, kind is *

data BoolAndSomethingElse a = 
    False' a | True' a
    deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a)  = True' (f a)

data BoolAndMaybeSomethingElse a = 
    Falsish | Truish a
    deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish    
    fmap f (Truish a) = Truish (f a)

--4 no becuase f is kind * -> * making Mu *-> * -> *
newtype Mu f = InF { outF :: f (Mu f)}

--5. No, because kind is *
data D = D (Array Word Word) Int Int

data Sum' b a = 
    First' a
  | Second' b
  deriving (Eq, Show) 

instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

data Company a c b = 
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk a)  = Desk a
    fmap f (Bloor b) = Bloor (f b) 

data K a b =
    K a

instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b =
    K' a

instance Functor (Flip K' a) where
    fmap f (Flip (K' a))  = Flip $ K' (f a)

data EvilGoateeConst a b = 
    GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a =
    LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (f <$> fa)

data Parappa f g a = 
    DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

data IgnoreOne f g a b = 
    IgnoreSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (f <$> gb)

data Notorious g o a t =
    Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a)  where
    fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

data List' a = 
    Nil'
  | Cons' a (List' a)
  deriving (Eq, Show)

instance Functor List' where
    fmap _ Nil' = Nil'
    fmap f (Cons' a lista) = Cons' (f a) (fmap f lista)

data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
            
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats ga gb gc)  = MoreGoats (fmap f ga)
                                             (fmap f gb)
                                             (fmap f gc)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read func)      = Read (f <$> func)

