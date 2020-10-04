import           Text.Regex.Posix               ( (=~) )
import           Data.Char

globToRegex :: String -> Bool -> String
globToRegex cs caseSensitive = case caseSensitive of
  False -> '^' : globToRegex' cs ++ "$"
  True  -> '^' : globToRegex' (map toLower cs) ++ "$"

globToRegex' :: String -> String
globToRegex' ""                   = ""
globToRegex' ('*'           : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?'           : cs) = "." ++ globToRegex' cs
globToRegex' ('[' : '!' : c : cs) = "[^" ++ c : charClass cs
globToRegex' ('['       : c : cs) = '[' : c : charClass cs
globToRegex' ('['           : _ ) = error "unterminated character class"
globToRegex' (c             : cs) = escape c ++ globToRegex' cs

charClass :: String -> String
charClass (']' : cs) = ']' : globToRegex' cs
charClass (c   : cs) = c : charClass cs
charClass []         = error "unterminated character class"

-- escape function ensures that the regexp engine will not interpret
-- certain characters as pieces of regular expression syntax.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"
