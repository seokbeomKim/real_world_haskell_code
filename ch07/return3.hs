-- file: ch07/return3.hs

-- return을 이해하기 가장 좋은 예인 것 같다. 단순한 람다식인 let two = 2에
-- 의해서 변수 two에 2가 저장된 상태이고, IO Monad를 반환하는 return 이지만 <-
-- 를 통해서 전달 받기에, IO Monad 객체를 제외한 순수 값만 one으로 전달받을 수
-- 있다. 이에, (one + two) 가 가능하며, 만약 단순하게 one = return 1을 한다면, 
-- one은 IO 1 이 되므로 순수 값과의 연산은 불가능하다.
returnTest :: IO ()
returnTest = 
    do one <- return 1
       let two = 2
       putStrLn $ show (one + two)
