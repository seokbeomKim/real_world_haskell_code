{-
하스켈에서는 type Pair a = (a, a)와 같은 문법으로 객체 생성하는 것을 허용하지 않기 때문에
아래와 같이 Pair 타입을 정의하여 사용해야 한다.
여담으로, 아래 예제에서 사용한 fmap은 이미 lifting operation이라는 것을 알 수 있다.
fmap의 타입을 살펴보면 아래와 같다.

fmap :: Functor f => (a -> b) -> f a -> f b
-}
data Pair a = Pair a a deriving Show
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
