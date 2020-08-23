-- Using the command framework from the earlier section A Simple
-- Command-Line Framework, write a program that prints the first word
-- of each line of its input.

import System.Environment (getArgs)

interactWith :: Show a => (String -> a) -> FilePath -> IO ()
interactWith function inputFile = do
  input <- readFile inputFile
  print $ function input

firstWord lines = head $ words lines

firstWordOfAll :: String -> [String]
firstWordOfAll input = fmap firstWord (lines input)

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input] -> interactWith function input
        _ -> putStrLn "error: an argument needed"

myFunction = firstWordOfAll
