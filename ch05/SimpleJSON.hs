-- file: ch05/SimpleJSON.hs
-- module declaration
module SimpleJSON
  ( JValue (..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull,
  )
where

-- 기본 Haskell value들을 JValue 로 쉽게 바꿔주는 것을 확인할 수 있다. 그리고
-- JValue에서 기본 타입으로 바꾸는 반대의 과정은 패턴 매칭을 이용하여 구현할 수
-- 있다.
data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

-- define accessor

-- Extract a string from a JSON value
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull