module MyLib where

data Mood = Blah | Woot deriving Show
--type constructor = Mood
--Blah or Woot
--Data constructor in type field, should be a Type constructor

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

--not True && True
--not (x == 6)
--OK
--["Merry"] > ["Happy"]
--['1', '2', '3'] ++ "look at me!"

p = "Papuchon"

awesome = [p, "curry", ":)"]

s = "The Simons"

also = ["Quake", s]

allAwesome = [awesome, also]

--length :: [a] -> Int
--5
--3
--2
--5

-- (/) :: Fractional a => a -> a
-- Int `elem` Fractional == False
-- therefore 6 / lenght [1,2,3] errors out

-- 6 `div` length [1,2,3]
-- Bool; True
-- Bool; False

--True
--errors
--5
--False
--errors

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b,d), (a,c))
f x y = ((b,d), (a, c))
  where b = snd x
        d = snd y
        a = fst x
        c = fst y

--

x = (+)

f' xs = w `x` 1
  where w = length xs

x' = \x -> x

f'' (a, b) = a

--c
--b
--c
--d
