-- Write your own definition of the standard takeWhile function, first using
-- explicit recursion, and then foldr.

-- 1. using explicit recursion
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile filter (x : xs)
  | filter x == True = [x] ++ myTakeWhile filter xs
  | otherwise = []

-- 2. using foldr
myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' filter arr = foldr f [] arr
  where
    f elem acc
      | filter elem = elem : acc
      | otherwise = []