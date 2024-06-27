{-# LANGUAGE InstanceSigs #-}

module MyLib where

import Data.Char
import Control.Monad (join)

newtype Identity a
  = Identity {runIdentity :: a}
  deriving (Eq, Show)

newtype Compose f g a
  = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

exampleCompose :: Compose Maybe [] Int
exampleCompose = Compose (Just [1, 2, 3])

example2Compose :: Compose [] Maybe Int
example2Compose = Compose [Just 1, Nothing]

--------

instance
  (Functor f, Functor g) =>
  Functor (Compose f g)
  where
  fmap f (Compose fga) =
    Compose $ ((<$>) . (<$>)) f fga

-- GOTCHA! Exercise Time

instance
  (Applicative f, Applicative g) =>
  Applicative (Compose f g)
  where
  pure :: a -> Compose f g a -- Compose Maybe Maybe Int
  pure a = Compose $ (pure . pure) a

  (<*>) (Compose fgf) (Compose fga) = Compose $ ((<*>) <$> (fgf) <*> fga)

-- Exercises: Compose instances

instance
  (Foldable f, Foldable g) =>
  Foldable (Compose f g)
  where
  foldMap f (Compose fga) = foldr (\x acc -> (foldMap f x) <> acc) mempty fga

instance
  (Traversable f, Traversable g) =>
  Traversable (Compose f g)
  where
  traverse f (Compose fga) = Compose <$> traverse (traverse f) fga

--And now for something completely different

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap ::
    (a -> b) ->
    (c -> d) ->
    p a c ->
    p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c

data Deux a b = Deux a b
    deriving (Eq, Show)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
    deriving (Eq, Show)

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c
    deriving (Eq, Show)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei  a b
   deriving (Eq ,Show)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
    deriving (Eq, Show) 

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
    deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Bifunctor Either' where
   bimap f _ (Left' a)   = Left' (f a)
   bimap _ g (Right' b)  = Right' (g b)

--------

{- newtype Identity a
  = Identity {runIdentity :: a}
  deriving (Eq, Show)
 -}

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance (Functor m)
    => Functor (IdentityT m) where
        fmap f (IdentityT fa) =
            IdentityT (fmap f fa)

instance (Applicative m)
    => Applicative (IdentityT m) where
        pure a = IdentityT (pure a)

        (<*>) (IdentityT fg) (IdentityT fa) = IdentityT (fg <*> fa)

instance (Monad m) 
    => Monad (IdentityT m) where
        return = pure
        (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
        (>>=) (IdentityT fa) f = IdentityT $ fa >>= runIdentityT . f

