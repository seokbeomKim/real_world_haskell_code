-- file: ch07/actions2.hs
str2message :: String -> String
str2message input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

numbers :: [Int]
numbers = [1..10]

main = do str2action "Start of the program"

          -- mapM_ 함수는 map과 비슷하지만 전달하는 함수의 반환값이 IO ()라는
          -- 점이 특징이다. map의 경우는 순수 함수이기 때문에, do block 내에서 
          -- 곧바로 실행되지 못한다.
          -- map (str2action . show) numbers
          mapM_ (str2action . show) numbers
          str2action "Done!"