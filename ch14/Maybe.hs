-- maybe monad 구현하기. 책에 나와있는 예제를 통해 모나드에 대한
-- 개념을 이해하도록 한다.

instance Monad Maybe where
  Just x >>= k = k x
  Nothing >>= _ = Nothing

  Just _ >> k = k
  Nothing >> _ = Nothing

  return x = Just x
