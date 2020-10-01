-- myWork - a test file to check the features described in a serious
-- of csv* files.

-- A following module needs to set option ':set -package parsec'
import Control.Monad.Identity (Identity)
import Text.ParserCombinators.Parsec

-- 1. endBy & sepBy
command = endBy paths eol

-- sepBy function takes two functions as arguments: the first parses
-- some sort of content, while the second parses a separator. This
-- returns a list of all the content that it was able to parse.
paths = sepBy cell (char ':')

cell = many (noneOf ",\n\r")
eol = try (string "\n")

-- 2. <|>
myParser :: Parsec.Parsec String () (String, String)
myParser = do
  letters <- Parsec.many1 Parsec.letter
  Parsec.spaces
  digits <- Parsec.many1 Parsec.digit
  return (letters, digits)
