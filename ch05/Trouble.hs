-- file: ch05/Trouble.hs

-- When we omit explicit type information, we force the compiler to figure out
-- our intentions.

import Data.Char (toUpper)

-- 아래의 경우, 컴파일러는 정상적으로 타입 추론을 한다.
-- upcaseFirst :: String -> Char
upcaseFirst (c : cs) = toUpper c : cs -- forgot ":cs" here

-- camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))

testUndefined x = error "need to implement" >> undefined