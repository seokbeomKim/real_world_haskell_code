-- ch16/csv2.hs
import Text.ParserCombinators.Parsec

eol = char '\n'
line = sepBy cell (char',')
cell = many (noneOf ",\n")
csvFile = endBy line eol

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
