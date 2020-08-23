-- Use a fold (choosing the appropriate fold will make your code much
-- simpler) to rewrite and improve upon the asInt function from the
-- earlier sectionExplicit Recursion.

import Data.Char (digitToInt, isDigit)

-- recursive way
asInt :: String -> Int
asInt str = f 0 str
  where
    f acc (x : xs)
      | isDigit x = f (acc * 10 + digitToInt x) xs
      | x == '-' = negate (f 0 xs)
    f acc [] = acc

-- foldable way
asIntFold (x : xs)
  | elem '-' (x : xs) = (-1) * asIntFold xs
  | elem '.' (x : xs) = error "not a digit '.'"
asIntFold str = foldl f 0 str
  where
    f a b = a * 10 + digitToInt b

-- The asIntFold function uses error, so its callers cannot handle errors.
-- Rewrite the function to fix this problem

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "Cannot convert [] to Int"
asInt_either (x : xs)
  | elem '-' (x : xs) = fmap (* (-1)) (asInt_either xs)
  | elem '.' (x : xs) = Left "non-digit 'o'"
asInt_either str = Right (foldl f 0 str)
  where
    f a b = a * 10 + digitToInt b
