-- file: ch08/GlobRegex.hs
module GlobRegex (globToRegex) where

import Text.Regex.Posix((=~))

{- glob pattern을 이용하여 하스켈에서 정규식을 사용하는 방법에 대해 정리한다.
glob 패턴은 유닉스 셸이 사용하는 규칙으로 지정된 패턴을 찾을 때 사용하는 것으로서 
정규식과 비슷하다.
-}

-- 정규식은 반드시 anchor(^)로 시작함으로써 문장이 시작되는 것을 알려야한다.
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs 
globToRegex' ('[':_) = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) = c : charClass cs
charClass [] = error "unterminated character class"

escape :: Char -> String 
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
       where regexChars = "\\+()^$.{}]|"