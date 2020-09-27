-- ch3/passwd-al.hs

import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)

main = do
  -- Load the command-line arguments
  args <- getArgs

  -- argc 확인하기
  when (length args /= 2) $ do
    putStrLn "Syntax: passwd-al filename uid"
    exitFailure
