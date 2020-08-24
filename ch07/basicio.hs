-- file: ch07/basicio.hs
main = do
  putStrLn "Greetings! what is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

-- The $ operator is a bit of syntactic sugar that is equivalent to putting
-- everything after it inside a pair of parentheses.