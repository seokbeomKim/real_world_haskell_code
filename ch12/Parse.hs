module Parse where
-- file: ch10/Parse.hs
-- ch12예제에서 응용하므로 재 정리한다.

-- 10장에서 언급된대로 PGM (portable gray map)을 파싱하기 위한 파서를 구현한
-- 코드이다. PGM은 실제로는 포맷 한 개가 아니라 두 개로 구성되어 있다. (P2/P5)

import           Control.Applicative
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (chr, isSpace, isDigit)
import           Data.Int                   (Int64)
import           Data.Word

-- 아래 데이터 타입을 통해 현재 파싱 위치와 남은 ByteString 정보를 얻는다.
data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)

-- 아래와 같은 함수를 생각해볼 수 있다. 초기 state를 가지고 파싱을 시작하면,
-- 이후 오프셋 정보와 함께 튜플 형태로 함께 관리하는 방법이다.
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- 파싱에 실패할 경우 에러메시지를 출력할 수 있도록 위의 simpleParse를 간단하게
-- 변형할 수 있다.
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

{- 클라이언트 코드에서는 파서에 대해서만 신경쓸 뿐 실제 구현부에 대해서는 알
필요가 없다. 때문에 아래와 같이 Parse만 노출시키고 나머지 부분은 숨김으로서
클라이언트 코드에서의 접근을 막을 수 있다.

newtype 정의는 컴파일 타임 wrapper이므로 runtime에서의 오버헤드가 전혀 없다.
이후 클라이언트 코드에서 파서를 사용할 수 있도록 runParse 접근자를 구현할
것이다. -}
newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

-- 파서 예제 1: identity parser
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- 앞서 명시한 Parse wrapper를 조작하여 실제 runParse 함수를 사용할 수 있도록
-- unwrapping하는 함수이다.
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
  Left err          -> Left err
  Right (result, _) -> Right result

{- record syntax는 단순하게 접근자 함수를 위한 용도 외에도 유용하게 사용할 수
있다. 이미 할당되어 있는 값을 부분적으로 아래와 같이 바꿀수도 있다. -}
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState {offset = newOffset}

-- parse parseByte $ L8.pack "foo"로 테스트해볼 수 있다.
parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
      Just (byte, remainder) -> putState newState ==> \_ -> identity byte
        where
          newState = initState {string = remainder, offset = newOffset}
          newOffset = offset initState + 1

-- getState와 putState에서 한 가지 눈여겨 볼 것은, 함수의 반환이
-- 반드시 값이 아니라는 것이다. 즉, 리턴 값이 함수인 경우도 가능하기에
-- currying을 위해서 아래와 같이 접근자들을 구현할 수 있다. 즉,
-- 함수들을 이용하여 또 다른 함수를 정의하는 방법이다.
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err =
  Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState = case runParse firstParser initState of
      Left errMessage -> Left errMessage
      Right (firstResult, newState) ->
        runParse (secondParser firstResult) newState

instance Functor Parse where
  fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
      then parseByte ==> \b -> (b :) <$> parseWhile p
      else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
  if null digits
  then bail "no more input"
  else let n = read digits
           in if n < 0
              then bail "integer overflow"
              else identity n

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()


assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err
