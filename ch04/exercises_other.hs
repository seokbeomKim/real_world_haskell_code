-- We can reuse this code which is given in
-- the previous chapter
interactWith function inputFile outputfile = do
    input <- readFile inputFile
    writeFile outputfile (function input)

main = mainWith myFunction
	where myFunction = getFirstWordsOfFile
    	      mainWith function = do
    	      args <- getArgs
    	      case args of
        	      [input,output] -> interactWith function input output
	       	      _ -> putStrLn "error: exactly two arguments needed"

firstWord :: String -> String
firstWord line = head (words line)

firstWordsOfFile :: [String] -> String
firstWordsOfFile [] = ""
firstWordsOfFile (x:xs) = (firstWord x) ++ " " ++ (firstWordsOfFile xs)

getFirstWordsOfFile :: String -> String
getFirstWordsOfFile fileStr = firstWordsOfFile (lines fileStr)
