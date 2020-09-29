-- ch13/funcrecs.hs
-- 앞서 funcrecs에서 구현했던 부분을 조금 더 응용한 예제이다.

data FuncRec =
  FuncRec {name :: String,
           calc :: Int -> Int,
           namedCalc :: Int -> (String, Int)}

-- name과 함수를 전달받아 tuple로 구성한 데이터를 생성한다.
mkFuncRec :: String -> (Int -> Int) -> FuncRec
mkFuncRec name calcFunc =
  FuncRec {name = name,
           calc = calcFunc,
           namedCalc = \x -> (name, calcFunc x)}

plus5 = mkFuncRec "plus5" (+ 5)
always0 = mkFuncRec "always0" (\_ -> 0)

-- 이 예제에서 중요한 것은, namedCalc 필드를 수정하지 않고 name 필드만
-- 수정하여 원하는 결과를 얻었다는 점이다.
