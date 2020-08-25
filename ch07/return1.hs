-- file: ch07/return1.hs
import Data.Char(toUpper)

-- isGreen 의 경우 pure function 이 아닌 Bool을 반환하는 action이다. 때문에,
-- IO를 Monad Wrapper라고 생각했을 때, ((toUpper . head $ inpStr) == Y) 라는
-- 순수 함수를 반환하는 Monad 반환 함수로 이해할 수 있다.
isGreen :: IO Bool
isGreen =
    do putStrLn "Is green you favorite color?"
       inpStr <- getLine
       return ((toUpper . head $ inpStr) == 'Y')