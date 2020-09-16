-- concat :: Foldable t => t [a] -> [a]
-- 예를 들어, concat [[1,2,3,4],[5,6,7,8]] 은 [1,2,3,4,5,6,7,8] 이 된다.

-- s@(first:reset) 문법 확인
-- 아래와 같이 응용할 수 있다.
testAsSymbol :: [String] -> Int

-- @ "as" 심볼이며 ps@(p:pt)는 아래의 의미를 갖는다.
-- 1. the list: ps
-- 2. list's head: p
-- 3. list's tail: pt
testAsSymbol xs@(x:l) = length x
