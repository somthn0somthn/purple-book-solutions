module Reverse (rvrs, main) where


rvrs :: String -> String
rvrs str = z ++ " " ++ y ++ " " ++ x 
  where
    x = take 5 str 
    y = take 2 $ drop 6 str
    z = drop 9 str

main :: IO ()
main = print x
    where x = rvrs "Curry is awesome"