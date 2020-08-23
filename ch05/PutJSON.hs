module PutJSON where

import Data.List (intercalate)
import PrettyJson
import SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs ps = intercalate ", " (map renderPair ps)
    renderPair (k, v) = show k ++ ": " ++ renderJValue v

putJValue v = putStrLn (renderJValue v)

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r -> text r
  Nothing
    | mustEscape c -> hexEscape c
    | otherwise -> char c
  where
    mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where
    ch a b = (a, ['\\', b])