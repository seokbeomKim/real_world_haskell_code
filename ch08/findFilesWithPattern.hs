-- findFilesWithPatterns.hs

-- 앞서 GlobRegex_rety.hs 예제의 연장으로 파일 리스트를 읽어 패턴에
-- 맞는 파일들을 출력하는 코드를 간단하게 짜보도록 한다. 이 때, 입력은
-- glob expression을 이용하는 예제 코드를 일부 사용하도록 하자.

import           Text.Regex.Posix               ( (=~) )
import           Data.ByteString.Lazy.Char8     as L
import Data.Char

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
  let pat = patstr input
  return $ Prelude.filter (isPatternMatched pat) $ (fmap unpack $ L.lines files)

isPatternMatched :: String -> String -> Bool
isPatternMatched pat src = (src =~ pat) :: Bool

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""                   = ""
globToRegex' ('*'           : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?'           : cs) = "." ++ globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('['       : c : cs) = '[' : c : charClass cs
globToRegex' ('['           : _ ) = error "unterminated character class"
globToRegex' (c             : cs) = escape c ++ globToRegex' cs

charClass :: String -> String
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c   : cs) = c : charClass cs
charClass []         = error "unterminated character class"

-- escape function ensures that the regexp engine will not interpret
-- certain characters as pieces of regular expression syntax.
escape :: Char -> String
escape c | c `Prelude.elem` regexChars = '\\' : [c]
         | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"
