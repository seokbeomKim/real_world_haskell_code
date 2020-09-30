-- MovieReview.hs 에서 테스트한 Function Lifting 을 테스트하기 위한
-- 예제를 작성한다. liftM 은 기본적으로 인자와 함수의 결과를 특정
-- 모나드로 변형하는 역할을 갖는다.

import Control.Monad

data DataToLift = DataToLift {
    name :: String
  , nickname :: String
} deriving (Eq, Show)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing
