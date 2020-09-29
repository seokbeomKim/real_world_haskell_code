-- myWorkWithMap.hs
--
import Text.Printf (printf)

-- passwdmap 예제를 보고 추가로 연습하기 위한 예제이다.
data YourName = YourName {
  realname :: String,
  nickname :: String }
  deriving (Eq, Ord)

instance Show YourName where
  show yn = printf "%s(%s)"
            (realname yn) (nickname yn)
