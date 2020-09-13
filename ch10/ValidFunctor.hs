-- file: ch10/ValidFunctor.hs

-- TreeMap에서 사용한 Functor 인터페이스 구현에서 몇 가지 제약사항을
-- 설명하기 위한 예제이다.

-- 아래와 같이 type constructor에 제약사항을 두는 것은 금지된다.
-- 예전 하스켈 버전에서는 가능했던 것으로 보이나, 현재는 load조차 되지 않는다.
data Eq a => Bar a = Bar as

instance Functor Bar where
  fmap f (Bar a) = Bar (f a)
