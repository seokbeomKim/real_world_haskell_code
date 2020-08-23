-- file: ch04/IntParse.hs

import Data.Char (digitToInt)
loop :: Int -> String -> Int
loop _ [] = 0
loop acc (x:xs) = loop acc' xs
  where acc' = acc * 10 + digitToInt x -- This could be rewritten into
                                       -- loop acc (x:xs) = let acc' =
                                       -- acc * 10 + digitToInt x in
                                       -- loop acc' xs
