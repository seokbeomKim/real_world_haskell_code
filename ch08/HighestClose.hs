-- file: ch08/HighestClose.hs
import qualified Data.ByteString.Lazy.Char8 as L

-- 아래의 !!4의 의미는 리스트에서 k번째를 가져온다는 의미를 갖는다.
closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str = case L.readInt str of 
        Nothing -> Nothing
        Just (dollars, rest) -> 
            case L.readInt (L.tail rest) of 
                Nothing -> Nothing 
                Just (cents,more) -> Just (dollars * 100 + cents)

-- 아래에서 Nothing: 을 사용하는 이유는 비어있는 리스트에 대해서 empty list 에러가 나는 것을
-- 방지하기 위해 Nothing:로 채우는 역할을 한다.
highestClose = maximum . (Nothing:) . map closing . L.lines
highestCloseFrom path = do 
    contents <- L.readFile path
    print (highestClose contents)                