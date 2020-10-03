deriving (Monad)
module Supply
  ( Supply
  , next
  , runSupply) where

import Control.Monad.State

-- s: the type of unique values we are going to supply
-- a: usual type parameter that we must provide in order to make our type a monad
newtype Supply s a = S (State [s] a)

-- runSupply는 실행함수로서 클라이언트 코드에서 직접적으로 supply를
-- 생성하지 않도록 막기 위한 인터페이스이다.
runSupply :: Supply s a -> [s] -> (a, [s])

-- 리스트에서 하나를 가져와 consumer 코드에 전달한다.
next :: S $ do st <- get
               case st of
                 [] -> return Nothing
                 (x:xs) -> do put xs return (Just x)
