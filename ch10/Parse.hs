-- file: ch10/Parse.hs
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (isSpace)
import           Data.Int                   (Int64)

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
} deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

{- 파싱에 실패할 경우 에러메시지를 출력할 수 있도록 위의 simpleParse를 간단하게
변형할 수 있다. -}
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

{- 클라이언트 코드에서는 파서에 대해서만 신경쓸 뿐 실제 구현부에 대해서는 알
필요가 없다. 때문에 아래와 같이 Parse만 노출시키고 나머지 부분은 숨김으로서
클라이언트 코드에서의 접근을 막을 수 있다.

newtype 정의는 컴파일 타임 wrapper이므로 runtime에서의 오버헤드가 전혀 없다.
이후 클라이언트 코드에서 파서를 사용할 수 있도록 runParse 접근자를 구현할
것이다. -}
newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err         -> Left err
        Right (result,_) -> Right result
