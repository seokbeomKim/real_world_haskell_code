-- file: ch08/GlobRegex.hs
-- ** Translating a glob pattern into a regular expression

{- Note
~~~~~~~
glob pattern을 이용하여 하스켈에서 정규식을 사용하는 방법에 대해 정리한다.
glob 패턴은 유닉스 셸이 사용하는 규칙으로 지정된 패턴을 찾을 때 사용하는 것으로서
정규식과 비슷하다.
-}

module GlobRegex
  ( globToRegex
  , matchesGlob
  )
where

import           Text.Regex.Posix               ( (=~) )
import           Data.Char


-- 정규식은 반드시 anchor(^)로 시작함으로써 문장이 시작되는 것을 알려야한다.
-- 그리고 패턴의 끝은 ($)을 이용하여 나타낸다.
-- 아래 함수를 이용하여 아래와 같이 패턴 매칭을 이용할 수 있다.
{-
"foo.c" =~ globToRegex "f??.c" :: Bool
"test.c" =~ globToRegex "t[ea]s*" :: Bool
"taste.txt" =~ globToRegex "t[ea]s*" :: Bool
-}

-- A function takes a glob pattern and returns its representation as a
-- regular expression. Both glob patterns and regexps are text
-- strings, so the type that our function ought to have seems clear.
globToRegex :: String -> String

-- The regular expression that we generate must be anchored, so that
-- it starts matching from the beginning of a string and finishes at
-- the end.
globToRegex cs sensitive = '^' : globToRegex' ss ++ "$"
  where ss = if sensitive == True then map toLower cs else cs

-- 하스켈의 패턴 매칭을 이용하여 glob 형식의 패턴 매칭을 POSIX 정규식 방법으로
-- 변환해준다.
-- 명령형 프로그래밍(Imperative Language)에서 globToRegex' 함수는 통상 우리가
-- 루프라고 표현하는 것과 같다. 하지만 하스켈이나 Scheme 등의 언어를 사용할 경우
-- 이를 <tail recursion> 을 통해 표현해줘야 한다.
globToRegex' :: String -> String
globToRegex' ""                   = ""

globToRegex' ('*'           : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?'           : cs) = '.' : globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('['       : c : cs) = '[' : c : charClass cs
globToRegex' ('['           : _ ) = error "unterminated character class"

-- globToRegex' 함수의 경우는 tail recursion이 아니다. 순수하게 함수 만으로 반복되지
-- 않고 escape 함수의 결과와 함께 (++) 함수가 적용되기 때문이다.
globToRegex' (c             : cs) = escape c ++ globToRegex' cs

-- charClass는 helper function으로서 해당 문자가 올바르게 마무리되었는지 확인한다.
-- 단순하게 globToRegex' 함수 내에서 '[' 로 시작하는 패턴이 올바르게 ']' 패턴으로
-- 닫히고 있는지 확인하는 함수이다.
charClass :: String -> String
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c   : cs) = c : charClass cs
charClass []         = error "unterminated character class"

-- escape 함수는 regexp 엔진이 정규식에 사용하는 특정 캐릭터들을
-- 해석하지 않도록 방지한다.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat False
