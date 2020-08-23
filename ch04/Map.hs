import Data.Char (toUpper)

-- an example of transforming with specific function
transform :: (t -> a) -> [t] -> [a]
transform _ [] = []
transform f (x:xs) = [f x] ++ transform f xs

-- uppercase
uppercase :: String -> String
uppercase [] = []
uppercase (x:xs) = [toUpper x] ++ uppercase xs

-- using map, we can write a function doing same thing more easily
square2 xs = map squareOne xs
  where squareOne x = x * x

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []
