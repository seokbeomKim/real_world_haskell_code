import qualified Data.ByteString.Lazy      as L
import           Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.ByteString.Lazy.UTF8 as UL

-- concat :: Foldable t => t [a] -> [a]
-- 예를 들어, concat [[1,2,3,4],[5,6,7,8]] 은 [1,2,3,4,5,6,7,8] 이 된다.

-- s@(first:reset) 문법 확인
-- 아래와 같이 응용할 수 있다.
testAsSymbol :: [String] -> Int

-- @ "as" 심볼이며 ps@(p:pt)는 아래의 의미를 갖는다.

-- 1. the list: ps
-- 2. list's head: p
-- 3. list's tail: pt
testAsSymbol xs@(x:l) = length x

-- 데이터 타입 정의 방법에 대하여 (Greymap 참고)
data Greymap = Greymap {
    greyWidth    :: Int
    , greyHeight :: Int
    , greyMax    :: Int
    , greyData   :: L.ByteString
}

-- 위 데이터 타입에 대하여, Show instance를 구현하여 데이터 구조에 대해 출력될
-- 메시지를 지정할 수 있다.
instance Show Greymap where
    show (Greymap w h m d) = "[Greymap info] width = " ++ show w ++ ", height = "
                                ++ show h ++ ", max = " ++ show m ++ ", data = " ++ show d

-- Hoogle을 이용하여 String -> ByteString으로 변환 가능한 함수를 찾는다.
x = Greymap 10 20 30 (fromString "qwer")

-- as symbol(@) 연습하기 위해 아래와 같이 String에 대한 배열을 Split 한 것으로
-- 임의 함수를 만든다.
-- let .. in 구문은 where 구문의 반대 시퀀스를 갖는다.
testSplitString :: L.ByteString -> String
testSplitString str = let len = UL.length str
                          both@(first,_) = L.splitAt 1 str
                      in if len < 4
                         then
                            show "less than 4"
                         else
                            show first
