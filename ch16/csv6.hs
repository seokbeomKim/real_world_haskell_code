-- csv6.hs

-- =try= takes one function, a parser, and applies it. When trying on
-- the left side of <|>, Prasec will try the option on the right even
-- if the left side failed after consuming some input.

import Text.ParserCombinators.Parsec
csvFile = endBy line eol
line = sepBy cell (char ',')

-- many p applies the parser p zero or more times. Returns a list of
-- the returned values of p.
cell = many (noneOf ",\n\r")

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
