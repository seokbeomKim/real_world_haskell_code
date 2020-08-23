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