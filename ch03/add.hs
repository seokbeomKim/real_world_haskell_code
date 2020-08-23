-- file: ch03/add.hs
add a b = a + b

sumList (x:xs) = x + sumList xs
sumList [] = 0
