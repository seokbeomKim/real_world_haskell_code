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

{- 아래 타입들은 어떠한 경우에 사용하는 건가?
http://learnyouahaskell.com/making-our-own-types-and-typeclasses

TestData3과 같이 데이터 타입을 정의할 수도 있지만, 통상적으로는 data constructor
뒤에 괄호를 두고 record syntax를 이용하여 값을 채워넣는다.
-}
data TestData1 = TestData1 {
    title   :: String
  , tlength :: Int
} | TestData2 {
    author :: String
  , year   :: Int
} | TestData3 String Int
  deriving (Eq, Show)

data TestD = TestD
            | TestD2

newtype FunctionType a = FT {
  func :: TestData1 -> Either String (a, String)
}

modifyLength :: TestData1 -> Int -> TestData1
modifyLength initData newVal =
    initData { tlength = newVal }
