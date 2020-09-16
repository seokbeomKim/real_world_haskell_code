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
                    -- TODO as symbol(@) 및 let-in 구문 연습하기
                    then Nothing
                    else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

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
