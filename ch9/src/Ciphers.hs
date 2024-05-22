module Ciphers where

import Data.Char

--ord 'a' == 97
--ord 'z' == 122 

--(new letter val % 122) + 96

cesearShift :: Int -> Char -> Char
cesearShift x c = chr $ modLetterInt letterInt
  where
    letterInt     = (x `mod` 26) + ord c

    modLetterInt x 
      | x > 122 = (x `mod` 122 ) + 96
      | otherwise = x

appCesearCyph :: Int -> String -> String
appCesearCyph x = map (cesearShift x . toLower)