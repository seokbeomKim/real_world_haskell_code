-- LargestFilename.hs

-- 입력된 파일을 읽어 가장 큰 파일의 이름을 찾는
-- 예제이다. HighestClose.hs 예제의 연장선으로서 같은 기능이지만 좀 더
-- 다른 방법으로 구현해보도록 한다.

-- 이전 바이너리를 읽을 때와는 달리, 형식이 있는 텍스트 파일이므로
-- ByteString을 이용하는 take n과 같은 함수는 사용하지 않는다.

import qualified Data.ByteString.Lazy.Char8 as L

data FileInfo = FileInfo { name :: L.ByteString
                         , size :: Int
                         } deriving (Eq, Show)

instance Ord FileInfo where
  (>) a b = (size a) > (size b)
  (<) a b = (size a) < (size b)
  (<=) a b = (size a) <= (size b)

main = do
  listOfFiles <- L.readFile =<< getLine
  return $ findLargestOne listOfFiles

findLargestOne context =
  maxSize (FileInfo (L.pack "") 0) (map getNameAndSize' (L.lines context))

maxSize :: FileInfo -> [FileInfo] -> FileInfo
maxSize c (x:xs) = if c < x
                   then maxSize x xs
                   else maxSize c xs
maxSize x [] = x

getNameAndSize' :: L.ByteString -> FileInfo
getNameAndSize' c =
  let str = L.words c
  in FileInfo (str !! 8) (read $ L.unpack (str !! 4) :: Int)

findName :: FileInfo -> L.ByteString
findName x = name x
