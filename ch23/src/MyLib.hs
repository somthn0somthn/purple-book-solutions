module MyLib where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import Test.QuickCheck
import Control.Arrow (ArrowApply(app))
import GHC.Read (list)

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $
        "intToDie got non 1-6 integer: "
        ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 1
        (d1, s1) = randomR (1,6) s
        (d2, s2) = randomR (1,6) s1
        (d3, _) = randomR (1,6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = 
    intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
   liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

--CONT from keep on rolling

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
           let (die, nextGen) = randomR (1,6) gen
           in go (sum + die) (count + 1) nextGen

--Roll your own

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= n = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
                  in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int
                 -> StdGen
                 -> (Int, [Die])
rollsCountLogged n gen = go 0 0 [] gen
  where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count dice gen
          | sum >= n = (count, dice)
          | otherwise =
               let (die, nextGen) = randomR (1, 6) gen
               in go (sum + die) 
                     (count + 1) (dice ++ [intToDie die]) 
                     nextGen

--write state for yourself

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Show (Moi s a) where
    show f = "Moi <function>"

instance (Semigroup a) => Semigroup (Moi s a) where
    (<>) (Moi f) (Moi g)  = Moi $ \s -> 
                                    let (a, s')    = f s
                                        (a', s'')  = g s'
                                    in  (a <> a', s'')

instance (Monoid a) => Monoid (Moi s a) where
    mappend = (<>)
    mempty = Moi $ \s -> (mempty, s)

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ \s ->
                       let (a, s') = g s
                       in (f a, s')

instance Applicative (Moi s) where
   pure a = Moi $ \s -> (a, s)
   (<*>) (Moi f) (Moi g) = Moi $ \s ->
                             let (a, s')   = g s
                                 (h, s'')  = f s'
                             in  (h a, s'')

instance Monad (Moi s) where
    return = pure
    (>>=) (Moi f) g = Moi $ \s -> 
                        let (a, s') = f s
                            (b, s'') = runMoi (g a) s'
                        in (b, s'')


instance (Arbitrary a, CoArbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Moi s a) where
    arbitrary = do
        func <- arbitrary 
        return $ Moi func

monoidMoiAssoc :: Int -> Moi Int String -> Moi Int String -> Moi Int String -> Bool
monoidMoiAssoc x a b c = runMoi (a <> (b <> c)) x == runMoi ((a <> b) <> c) x

monoidMoiLeftIdentity :: Int -> Moi Int String  -> Bool
monoidMoiLeftIdentity x a = runMoi (mempty <> a) x  == runMoi a x

monoidMoiRightIdentity :: Int -> Moi Int String  -> Bool
monoidMoiRightIdentity x a = runMoi (a <> mempty) x  == runMoi a x

functorMoiId :: Int -> Moi Int Int -> Bool
functorMoiId x moi = runMoi (id <$> moi) x == id (runMoi moi x)

functorMoiComposition :: (Int -> Int) -> (Int -> Int) -> Int -> Moi Int Int -> Bool
functorMoiComposition f g x moi =  runMoi ((f . g) <$> moi) x == runMoi ((fmap f . fmap g) moi) x

applicativeMoiId :: Int -> Moi Int Int -> Bool
applicativeMoiId x moi = runMoi (pure id <*> moi) x == runMoi moi x

applicativeMoiComposition :: Int 
                          -> Moi Int (Int -> Int)
                          -> Moi Int (Int -> Int)
                          -> Moi Int Int
                          -> Bool
applicativeMoiComposition x u v w = runMoi (pure (.) <*> u <*> v <*> w) x ==
                                       runMoi (u <*> (v <*> w)) x 

applicativeMoiHomomorphism :: (Int -> Int)
                           -> Int
                           -> Bool
applicativeMoiHomomorphism f x = runMoi (pure f <*> pure x) x == runMoi (pure (f x)) x

applicativeMoiInterchange :: Moi Int (Int -> Int)
                          -> Int
                          -> Bool
applicativeMoiInterchange u y = runMoi (u <*> (pure y)) y == runMoi (pure ($ y) <*> u) y

monadMoiRightIdentity :: Moi Int Int
                      -> Int
                      -> Bool
monadMoiRightIdentity moi x = runMoi (moi >>= return) x == runMoi moi x

monadMoiLeftIdentity :: Int
                     -> (Int -> Moi Int Int)
                     -> Bool
monadMoiLeftIdentity x f = runMoi (return x >>= f) x == runMoi (f x) x

monadMoiAssoc :: Int
              -> Moi Int Int
              -> (Int -> Moi Int Int)
              -> (Int -> Moi Int Int)
              -> Bool
monadMoiAssoc x m f g = runMoi ((m >>= f) >>= g) x == runMoi (m >>= (\x -> f x >>= g)) x

-----------

--FizzBuzz differently

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`  5 == 0 = "Buzz"
           | n `mod`  3 == 0 = "Fizz"
           | otherwise       = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo x y = go x y [] 
  where
        go x y ls
            | x == y    = (fizzBuzz y) : ls
            | x > y     = go x (y + 1) ((fizzBuzz y) : ls)
            | otherwise = go y x []

--Chapter exercises

--use Moi because of type clashes

get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ \x -> ((), s)

exec' :: Moi s a -> s -> s
exec' (Moi sa) s = snd $ sa s

eval' :: Moi s a -> s -> a
eval' (Moi sa) s = fst $ sa s

myModify' :: (s -> s) -> Moi s ()
myModify' f = Moi $ \s -> 
              let s'' = f s
              in ((), s'')
