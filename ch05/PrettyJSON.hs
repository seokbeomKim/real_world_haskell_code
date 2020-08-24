-- file ch05/PrettyJSON.hs
{-# LANGUAGE NoImplicitPrelude #-}

-- 이번 챕터에서 구현할 Prettify 모듈을 이용하여 실제로 값을 출력하는 클라이언트
-- 코드이다.

module PrettyJSON (renderJValue) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Int
import Numeric (showHex)
import PrettyStub (Doc, char, double, fsep, hcat, text, (<>))
import PrettyStub as PL
import SimpleJSON (JValue (..))
import Prelude (Bool (False, True), Char, Maybe (Just, Nothing), String)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))

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

enclose :: Char -> Char -> Doc -> Doc
enclose = undefined

-- enclose left right x = char left <> x <> char right

smallHex :: Int -> Doc
smallHex x =
  text "\\u"
    <> text (replicate (4 - length h) '0')
    <> text h
  where
    h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where
    a = (n `shiftR` 10) .&. 0x3ff
    b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c
  | d < 0x10000 = smallHex d
  | otherwise = astral (d - 0x10000)
  where
    d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item =
  enclose open close
    . fsep
    . punctuate (char ',')
    . map item

-- Our Prettify module provides the text,double,and string function
-- String을 직접 출력하기 보다 Doc 추상 타입을 이용하여 출력한다.
renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where
    field (name, val) =
      string name
        <> text ": "
        <> renderJValue val