-- exercise-04.hs The Prelude function concat concatenates a list of lists into
-- a single list and has the following type:
myConcat :: [[a]] -> [a]
-- Write your own definition of concat using foldr.
myConcat [[]] = error "It is not possible to concatenate empty lists!"
myConcat (x : xs) = foldr f x xs
  where
    f a b = b ++ a

-- This time, we will define concat using foldl
myConcat_foldl :: [[a]] -> [a]
myConcat_foldl [[]] = error "It is not possible to concatenate empty lists!"
myConcat_foldl (x : xs) = foldl (++) x xs