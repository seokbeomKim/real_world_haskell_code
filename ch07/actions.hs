-- file: ch07/actions.hs

{- 하스켈에서는 기본적으로 pure function 만을 이용하여 함수를 구현한다. 하지만,
IO 의 경우 모든 경우의 수에 대해서 예측할 수 없고 순수 함수로서 구현할 수
없기에, 이를 action 이라는 것으로 분류하여 함수와 비슷하지만 호출되었을 때
정해진 task들을 수행하는 것을 만들었다. 이러한 I/O action은 IO monad를 통해서
구현하였으며, monad 형태를 통해 순수 함수와의 연결을 자연스럽게 구현할 수 있다.
-}

-- 인자 하나를 받아 IO () 타입, 즉 action으로 반환한다.
str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2actions strings

printitall :: IO ()
printitall = runall actions

-- Take a list of actions, and execute each of them in turn.
runall :: [IO ()] -> IO ()
runall [] = return ()
runall (firstelem:remainingelems) = 
    do firstelem
       runall remainingelems

-- let 을 제외하고 do block에서는 반드시 I/O action을 가지고 있어야 한다.
main :: IO ()
main = do str2action "Start of the program"
          printitall
          str2action "Done!"        