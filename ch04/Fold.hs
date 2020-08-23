-- file: ch04/Fold.hs

foldl' _ zero [] = zero
foldl' step zero (x : xs) =
  let new = step zero x
   in new `seq` foldl' step new xs

-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
someFunc = (+)

hiddenInside x y = someFunc (x `seq` y)

-- incorrect: a variation of the above mistake
hiddenByLet x y z =
  let a = x `seq` someFunc y
   in anotherFunc a z

anotherFunc = seq

-- To strictly evaluate several values, chain applications of seq together.
chained x y z = x `seq` y `seq` someFunc z