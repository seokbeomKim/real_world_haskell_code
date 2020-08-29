-- file: ch08/append.hs
-- 재귀적으로 표현되었지만 "tail recursive"가 아닌 경우

(++) :: [a] -> [a] -> [a]

(x:xs) ++ ys = x : (xs ++ ys)
[] ++ ys = ys