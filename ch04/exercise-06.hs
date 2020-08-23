-- The Data.List module defines a function, groupBy, which has the following
-- type:

-- file: ch04/ch04.exercises.hs
-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- Use ghci to load the Data.List module and figure out what groupBy does, then
-- write your own implementation using a fold.

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' pred input = foldl foldFunc [] input
  where
    foldFunc [] elem = [[elem]]
    foldFunc acc elem
      | pred (head (last acc)) elem = (init acc) ++ [(last acc) ++ [elem]]
      | otherwise = acc ++ [[elem]]