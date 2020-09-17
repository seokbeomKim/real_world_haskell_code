-- file: ch10/PNM.hs
-- PGM 이미지를 나타내기 위한 데이터 타입을 정의한다.

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (isSpace)

-- PGM 이미지를 나타내기 위한 데이터 타입 정의
data Greymap = Greymap {
    greyWidth  :: Int
  , greyHeight :: Int
  , greyMax    :: Int
  , greyData   :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

-- PGM 파일의 헤더파일들을 처리하는 부분이다.
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        -- str에서 prefix(헤더에 해당하는)를 제거하고 불필요한 whitespace를 없앤다.
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- Nat: 자연수를 의미함
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
-- L8.readInt :: ByteString -> Maybe (Int, ByteString) getNat에서 한 가지
-- 포인트는, fromIntegral을 사용한다는 점이다. 사실상 readInt이므로 항상 num은
-- Integer 값을 가져온다. 그리고 getNat의 함수 원형에서도 Int를 튜플의 첫번째
-- 원소로 갖기 때문에, general type으로 변환할 필요가 없지만 이 예제에서는
-- fromIntegral을 사용하도록 구현되어 있다. fromIntegral의 경우 Int와 같은 타입
-- 데이터를 Num a와 같은 general한 것으로 바꿔주는 converter 역할을 한다.
getNat s = case L8.readInt s of
                Nothing -> Nothing
                Just (num, rest)
                    | num <= 0 -> Nothing
                    | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     -- str을 count 만큼으로 분리하고 이를 both@(prefix,_)에 할당한다.
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    -- 여기서 prefix의 길이가 count 값보다 작다면 아래와 같이 처리한다.
                    -- DONE as symbol(@) 및 let-in 구문 연습하기
                    then Nothing
                    else Just both

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

-- parseP5 함수는 ByteString을 읽어 Greymap으로 만들고 나머지를 나머지 튜플로
-- 갖는 데이터를 반환한다.
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
    case matchHeader (L8.pack "P5") s of
        Nothing -> Nothing
        Just s1 ->
            case getNat s1 of
                Nothing -> Nothing
                Just (width, s2) ->
                    case getNat (L8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNat (L8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of
                                            Nothing -> Nothing
                                            Just (_, s5) ->
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) ->
                                                        Just (Greymap width height maxGrey bitmap, s6)

-- 위의 parseP5 함수는 가독성이 떨어지고 불필요하게 반복되는 루틴이 있으므로
-- 리팩토링이 필요하다. 두 가지 공통점이 있는데, 하나는 마지막 인자를 받아 Maybe
-- 타입으로 반환한다는 점이고, 다른 하나는 Maybe 값을 분해하여 각 step 별 연산이
-- 성공&실패 했는지를 판단한다는 점이다. 아래 함수를 이용하여 위 두가지를 다룰
-- 수 있는 연산자를 하나 정의한다. 이전 Maybe 값에 따라, Maybe를 리턴하는 함수에
-- 인자로서 넘기고 해당 함수의 결과값을 Maybe 타입으로 다시 받는 연산자이다.
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

-- 아래 함수에서 중요한 것은 앞서 정의한 >>? 연산자를 이용하여 여러 함수콜을
-- chaining 한다는 것이다. 또한, 익명 람다 함수(anonymous lambda function)에
-- 대해 지양할 것을 책에서는 권장하고 있지만, 아래와 같은 코드는 짧다고
-- 판단되기에 작성되었다고 기술하고 있다. 되도록이면 익명 람다함수는 지양하자.
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s >>?
    \s -> skipSpace ((), s) >>?
    (getNat . snd) >>?
    skipSpace >>?
    \(width, s) -> getNat s >>?
    skipSpace >>?
    \(height, s) -> getNat s >>?
    \(maxGrey, s) -> getBytes 1 s >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

-- 위의 코드의 경우도 한 가지 문제가 있는데, tuple 사용을 하드코딩하고 있다는
-- 점이다. 파싱 도중에 어디까지 파싱을 완료했고 남은 데이터는 무엇인지 등의
-- 기능을 추가하려면 코드의 유연성이 더 보장되어야 한다. 이를 위해 ParseState 를
-- 정의하고 위 함수에서 문제가 되는 부분을 수정한다.
