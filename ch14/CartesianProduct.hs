-- file: CartesianProduct.hs

-- 일반적으로, 아래와 같이 튜플을 생성할 때 "bracketed notation"을
-- 사용한다. 하지만 대안으로 monadic 을 이용하는 방법도 가능하다.
comprehensive xs ys = [(x,y)|x<-xs, y<-ys]

monadic xs ys = do { x <- xs; y <- ys; return (x,y) }

blockyDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

blockyPlain xs ys =
  xs >>=
  \x -> ys >>=
  \y -> return (x, y)

blockyPlain_reloaded xs ys =
  -- WTF??
  concat (map (\x ->
                 concat (map (\y ->
                                return (x, y))
                        ys))
         xs)
