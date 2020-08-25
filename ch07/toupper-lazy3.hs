-- file: ch07/toupper-lazy3.hs
import Data.Char(toUpper)

main = do

  -- Neither readFile nor writeFile ever provide a Handle for you to
  -- work with, so there is nothing to ever hClose. readFile uses
  -- hGetContents internally, and the underlying Handle will be closed
  -- when the returned String is garbage-collected or all the input
  -- has been consumed.

  inpStr <- readFile "input.txt"
  writeFile "output.txt" (map toUpper inpStr)
