-- ListMonad.hs
-- 리스트 모나드에 대한 예제

returnSingleton :: a -> [a]
returnSingleton x = [x]

instance Monad [] where
  return x = []
  xs >>= f = concat (map f xs)
