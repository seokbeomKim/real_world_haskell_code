-- file: Logger.hs

-- chapter12에서 사용했던 globregex를 이용
-- 아래와 같이 Logger에 대해 abstraction은 유지한다.
module Logger
  (
    -- 아래와 같이 module을 이용해 불필효한 export를 없앨 경무
    -- 유저에게 simple interface 제공
    Logger
  , Log
  , runLogger
  , record
  ) where

import Control.Monad

-- Logger는 purely a type constructor이므로 사용자 코드에서 직접
-- 접근하여 오브젝트를 생성할 필요는 없다.
newtype Logger a = Logger { execLogger :: (a, Log) }

globToRegex :: String -> Logger String
globToRegex cs =
  globToRegex' cs Logger.>>= \ds ->
  return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' ('?':cs) =
  record "any" >>
  globToRegex' cs Logger.>>= \ds ->
  return ('.':ds)

type Log = [String]

-- runLogger Logger안에 있는 함수에 대해 client code에서 사용할 수
-- 있도록 제공하기 위해 구현한다.
runLogger :: Logger a -> (a, Log)
runLogger = execLogger

-- record 함수는 싱글톤 리스트를 생성한다.
record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
  return a = Logger (a, [])

(>>=) :: Logger a -> (a -> Logger b) -> Logger b
-- Monad를 이용할 경우 action과 monadic function을 결합할 수 있다.
m >>= k = let (a, w) = execLogger m
              n = k a
              (b, x) = execLogger n
          in Logger (b, w ++ x)
