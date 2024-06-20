{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module MyLib where

import Data.Attoparsec.Text (endOfLine)
import Text.Trifecta
import Text.RawString.QQ
import Control.Applicative
import Control.Monad
import Data.Ratio((%), numerator, denominator)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import qualified Data.Text as T
import Data.Text (Text, strip, pack, unpack)
import Data.Scientific (floatingOrInteger)
import GHC.Conc (par)
import Test.Hspec (context)

import Data.Time
import Data.Time.Format


--Parsing practice

stop :: Parser a
stop = unexpected "stop"

one' = one >> stop

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL s = 
    putStrLn ('\n' : s)

--Parsing practice

one :: Parser Char
one = char '1' <* eof

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

p123 :: String -> IO ()
p123 s = print $ parseString (string "123" <|> string "12" <|> string "1") mempty s

p123eof :: String -> IO ()
p123eof s = print $ parseString (string "123" <|> string "12" <|> string "1" <|> stop) mempty s

string' :: String -> Parser String
string' []       = return []
string' (x:xs)   = do
    _ <- char x
    _ <- string' xs
    return (x:xs)

string'' :: String -> Parser String
string'' []     = return []
string'' (c:cs) = (char c >>= \c -> (string'' cs) >>= \cs -> return (c:cs))

p123' :: String -> IO ()
p123' s = print $ parseString (string' "123" <|> string' "12" <|> string' "1") mempty s

p123'' :: String -> IO ()
p123'' s = print $ parseString (string'' "123") mempty s

--------

--Unit of success

p1234 :: String -> IO ()
p1234 s = print $ parseString (integer <* eof) mempty s

---

type NumberOrString = 
    Either Integer String 

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) 
        <|>  (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

eitherOr :: String
eitherOr = [r|
123
abc
456map
def
|]

someLetter :: Parser String
someLetter = some letter

data MyName = MyName String deriving Show

--Try trys

parseFraction' :: Parser Rational
parseFraction' = do
    numerator <- decimal
    v <- optional $ try (oneOf "./") 
    case v of
        Nothing -> return $ fromIntegral numerator
        Just '/' -> do
             denominator <- decimal
             if denominator == 0
                then fail "denominator can't be 0"
                else return (numerator % denominator)
        Just '.' -> do
            append <- decimal
            let appendLen = length (show append)
            return (fromIntegral numerator + (append % (10 ^ appendLen)))
        

--------------

--CONT FROM Marshalling from an AST

sectionJson :: ByteString
sectionJson = [r|
{  "section": {"host": "wikipedia.org"},
   "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = 
    TestData {
        section :: Host
      , what :: Color
    } deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _          = 
        fail "Expected an object for TestData"

instance FromJSON Host where
    parseJSON (Object v) =
        Host <$> v .: "host"
    parseJSON _ =
        fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) =
        ( Red <$> v .: "red")
     <|> (Blue <$> v  .: "blue")
     <|> (Yellow <$> v .: "yellow")

newtype Host =
    Host String
    deriving (Eq, Show)

type Annotation = String

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

data NumberOrString' =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString' where
    parseJSON (Number i) =
        case floatingOrInteger i of
            (Left _) ->
                fail "must be integral number"
            (Right integer) ->
                return $ Numba integer 
    parseJSON (String s) =
        return $ (Stringy s)
    parseJSON _ =
        fail "NumberOrString must\
        \ be number or string"

dec :: ByteString
    -> Maybe NumberOrString'
dec = decode

eitherDec :: ByteString
          -> Either String NumberOrString'
eitherDec = eitherDecode

{- main = do
    print $ dec "123"
    print $ dec "\"blah\""   -}  
----------------

--CONT FROM Chapter Exercises

data NumberOrString'' =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString'']
type Metadata = [NumberOrString'']

data SemVer = 
    SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)

instance Ord SemVer where
    compare (SemVer a b c _ _) (SemVer a' b' c' _ _)
      | (compare a a') /= EQ = compare a a'
      | (compare b b') /= EQ = compare b b'
      | (compare c c') /= EQ = compare c c'
      | otherwise = EQ

parseRelease :: Parser NumberOrString''
parseRelease = try (do
    release <- (NOSS <$> some alphaNum) <|> (NOSI <$> integer)
    _ <- optional (char '.')
    return release)

parseMetadata :: Parser NumberOrString''
parseMetadata = try (do
    meta <- (NOSS <$> some alphaNum) <|> (NOSI <$> integer)
    _ <- optional (char '.')
    return meta)

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    _ <- char '.'
    minor <- decimal
    _ <- char '.'
    patch <- decimal
    release <- option [] (char '-' *> some parseRelease)
    meta <- option [] (char '+' *> some parseMetadata)
    return $ SemVer major minor patch release meta

----------

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser [Char]
base10Integer = do
    v <- optional (char '-')
    case v of 
        Just '-' -> do
            negative <-many parseDigit
            return $ ('-' : negative)
        Nothing -> do
            positive <- many parseDigit
            return positive

-------

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea
                Exchange LineNumber
    deriving (Eq, Show)

parseThreeDigits :: Parser Int
parseThreeDigits = do
    digits <- count 3 digit
    return (read digits)

parseAreaCode :: Parser NumberingPlanArea
parseAreaCode = do
    _ <- optional (char '(')
    _ <- optional (string "1-")
    number <- parseThreeDigits
    _ <- optional (char ')')
    return number

parseExchange :: Parser Exchange
parseExchange = do
    skipMany (oneOf " ")
    _ <- optional (char '-')
    number <-parseThreeDigits
    _ <- optional (char '-')
    return number

parseLineNumber :: Parser LineNumber
parseLineNumber = do
    number <- count 4 digit
    return (read number)

parsePhoneNumber :: Parser PhoneNumber
parsePhoneNumber = do
    area <- parseAreaCode
    exchange <- parseExchange
    line <- parseLineNumber
    return $ PhoneNumber area exchange line

--gonna leave this incomplete & will come back to parsers later.
--It seems that Trifecta, which the chapter focused on,
--may not be the best parsing library. And that addressing these
--problems with megaparsec could make more sense

