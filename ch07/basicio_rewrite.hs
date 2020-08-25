-- file: ch07/basicio.hs
main = 
    putStrLn "Greetings! What is your name?" >>
    getLine >>=
        (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
