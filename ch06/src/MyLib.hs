module MyLib where
import Data.List (sort)

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Ord, Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date
  = Date DayOfWeek Int
  deriving (Show)

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') =
      weekday == weekday'
        && dayOfMonth == dayOfMonth'

--Exercises: Eq instances

data TisAnInteger = 
    TisAn Integer
    deriving (Show)

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) =
        x == y

data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') =
        x == x' && y == y'

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') =
        x == x'
    (==) (TisAString s) (TisAString s') =
        s == s'
    (==) _ _ = False

data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') =
        a == a' && b == b'

data Tuple a b = 
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') =
        a == a' && b == b'

data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') =
        x == x'
    (==) (ThatOne s) (ThatOne s') =
        s == s'
    (==) _ _ = False

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') =
        x == x'
    (==) (Goodbye s) (Goodbye s') =
        s == s'
    (==) _ _ = False

--Tuple Experiment
--provides a tuple from the result of two integrals 
--being `div` and `mod`, or `quot` and `rem`

--Will they work?
--yes
--yes
--no
--yes

--Chapter Exercises
--c
--b
--a 
--c
--a

--Does it type check
--no, no instance of show

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

--no, no Eq instance

data Mood = Blah
          | Woot deriving (Show, Ord)

instance Eq Mood where
    (==) Woot Woot = True
    (==) Blah Blah = True
    (==) _    _    = False

settleDown x = if x == Woot
                  then Blah
                  else x

--Blah or Woot
--types dont check, Num Mood instance doesnt exist
--fails, no Ord instance derived

--yes, it compiles

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

--no, Rocks and Yeah object not properly defined
phew = Papu (Rocks "chases") (Yeah True)

--yes
truth = Papu (Rocks "chomskydoz") (Yeah True)

--yes
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

--no, no Ord instance derived, has to be derived manually

--Match the types
--cant replace
--cant replace
--can replace
--can replace
f :: RealFrac a => a
f = 1.0

--can replace
freud :: Ord a => a -> a
freud x = x

--can replace
freud' :: Int -> Int
freud' x = x

--cant replace
myX = 1 :: Int

sigmund' :: Num a => a -> Int
sigmund' x = myX

--can replace
jung :: [Int] -> Int
jung xs = head (sort xs)

--can replace
young :: Ord a => [a] -> a
young xs = head (sort xs)

--cant replace

--Type-Kwon-Do Two: Electric typealoo
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = x == b
    where x = f a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f int a = (f a) + (f a)