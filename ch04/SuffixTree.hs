-- file: ch04/SuffixTree.hs

-- pattern xs@(_x:xs') is called an as-pattern, and it means "bind the variable
-- xs to the value that matches the right side of the @ symbol."
import Data.List (tails)

suffixes :: [a] -> [[a]]
suffixes xs@(_ : xs') = xs : suffixes xs'
suffixes _ = []

-- as-pattern makes our code more readable.... seriously?
noAsPattern :: [a] -> [[a]]
noAsPattern (x : xs) = (x : xs) : noAsPattern xs
noAsPattern _ = []

suffixes2 xs = init (tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)