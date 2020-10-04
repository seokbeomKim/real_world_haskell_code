-- findFilesWithPatterns.hs

-- 앞서 GlobRegex_rety.hs 예제의 연장으로 파일 리스트를 읽어 패턴에
-- 맞는 파일들을 출력하는 코드를 간단하게 짜보도록 한다. 이 때, 입력은
-- glob expression을 이용하는 예제 코드를 일부 사용하도록 하자.

import           Text.Regex.Posix               ( (=~) )
import           Data.ByteString.Lazy.Char8     as L

main = do
  return =<< getPatternMatchedFiles =<< getUserInput

type FileName = String
type PatternString = String
data UserInput = UserInput { filename :: FileName
                           , patstr :: PatternString
                           } deriving (Eq, Show)

getUserInput :: IO UserInput
getUserInput = do
  print $ "file name: "
  filename <- getLine
  print $ "pattern: "
  patstr <- getLine
  return $ UserInput filename patstr

getPatternMatchedFiles :: UserInput -> IO [String]
getPatternMatchedFiles input = do
  files <- L.readFile $ filename input
  return $ fmap unpack (L.lines files)
