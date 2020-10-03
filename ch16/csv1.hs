-- Simple CSV Parsing
import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated by
 the end-of-line character -}
csvFile :: GenParser Char st [[String]]
-- Type definition is meaning that the type of the input is a sequence
-- of characters, which is exactly what a Haskell string is, since
-- String is the same as [Char].
csvFile = do result <- many line
             eof
             return result

line :: GenParser Char st [String]
line =
  do result <- cells
     eol
     return result

-- Build up a list of cells. Try to parse the first cell.
-- Then figure out what ends the cell.
cells :: GenParser Char st [String]
cells =
  do first <- cellContent
     next  <- remainingCells
     return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) <|> (return [])
cellContent :: GenParser Char st String
cellContent =
  many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
