module MyLib where

import Control.Applicative
import Data.Char
import Data.Maybe

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

--NOTE :: applicative of functions lets you apply arguments in 
--parallel to two functions -> then to potentially combine the 
--results - this is a cool example that will test for either of two
--conditions applied to an arg 
{- 
(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool
(<||>) = liftA2 (||)
 -}

boopDoop :: Num a => a -> a
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

--Warming up

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
    a <- cap
    b <- rev
    return (a, b)



--Ask

ask :: Reader a a
ask = Reader $ id

--Reading comprehension

myLiftA2 :: Applicative f =>
         (a -> b -> c)
      -> f a -> f b -> f c
myLiftA2 g fa fb = g <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader $ \x -> f x

newtype Reader r a =
    Reader { runReader :: r -> a}

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
    pure a = Reader $ const a
    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
    return = pure
    (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r

--------

newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person =
    Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
   } deriving (Eq, Show)

data Dog =
   Dog {
     dogsName :: DogName
   , dogsAddress :: Address
   } deriving (Eq, Show)


getDogRM' :: Person -> Dog
getDogRM' = runReader $ (Reader address) >>= func 
  where
    func :: Address -> Reader Person Dog
    func a  = Reader $ \person -> Dog (dogName person) a

--Chapter Exercises
--A warm-up stretch

x = [1..3]
y = [4..6]
z = [7..9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> Maybe (Integer, Integer)
x3 n = (,) <$> z' n <*> z' n

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry' (+) 

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing = a
fromMaybe' _ (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]

    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> ys)
    print $ bolt 7
    print $ fmap bolt z

    putStrLn "sequenceA on list of funcs"
    print $ sequenceA [(>3), (<8), even] 7

    putStrLn "exercises"
    print $ foldr (&&) True $ sequA 6

    print $ sequA (fromMaybe' 0 s')
    print $ bolt (fromMaybe' 0 ys)

--13:30