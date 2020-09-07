-- ch10/myTest.hs
-- 예제 보면서 함수 구현 테스트

data Testmap = Testmap {
    a :: Int
  , b :: Int
  , c :: String
} deriving (Eq)

instance Show Testmap where
    show (Testmap a b c) =
        c ++ "a : " ++ (parseInt a)
        ++ ", b : " ++ (parseInt b)

parseInt :: Int -> String
parseInt = show

getFirstA :: Testmap -> Int
getFirstA (Testmap a _ _) = a
