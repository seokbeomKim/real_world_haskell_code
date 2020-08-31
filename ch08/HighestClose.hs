-- file: ch08/HighestClose.hs
import qualified Data.ByteString.Lazy.Char8 as L

-- This function obtains a closing price from one line of data
-- 아래와 같이 연산 가능하며, 인덱스는 0부터 시작한다.
-- Prelude> (!!4) $ [1,2,3,4]
-- *** Exception: Prelude.!!: index too large
-- Prelude> (!!3) [1,2,3,4]
-- 4

closing = readPrice . (!!4) . L.split ','

-- 여기서 L.readInt는 정수를 parse 한다. L.readInt의 경우 정수를 읽은 뒤 
-- 남은 것은 string 형태로 반환하며 이 때문에 Maybe (Int, ByteString) 타입으로 반환한다.
-- 이에 L.tail rest 의 경우 앞 한글자를 제외한 나머지 ByteString을 남기게 되므로
-- "19.80"은 Just (19, ".80")으로, 그리고 다시 "80"으로 dollars, cents가 할당되게 된다.
readPrice :: L.ByteString -> Maybe Int
readPrice str = case L.readInt str of 
    Nothing -> Nothing
    Just (dollars,rest) -> 
        case L.readInt (L.tail rest) of 
            Nothing -> Nothing
            Just (cents,more) ->
                Just (dollars * 100 + cents)

-- 아무런 데이터가 없는 경우는 L.empty를 이용하여 비어있는 파일의 경우를 테스트할 수 있다.
highestClose = maximum . (Nothing:) . map closing . L.lines
highestCloseFrom path = do
    contents <- L.readFile path
    print $ highestClose contents   