-- file: ch08/SumFile.hs
-- 텍스트 파일의 숫자를 모두 더한 값을 출력한다.

-- 파일에 대한 I/O로 String 타입이 기본이지만 효율적이지 않다.
-- 리스트의 각 요소들은 각각 개별적으로 할당되어 일정한 오버헤드를 갖기 때문에,
-- 메모리 소모나 성능에 영향을 끼친다.
-- 그렇기 때문에 String 대신에 bytestring을 이용하는 것이 훨씬 효율적이다.

main = do
    contents <- getContents
    print (sumFile contents)
    where sumFile = sum . map read . words

{- sumFile 에 대한 참고 자료

How to execute:
 ./SumFile
 1 2 3 4
^D
 10

*Main> map read . words $ "1 2 3 4"
[*** Exception: Prelude.read: no parse
*Main> map read . words $ "1 2 3 4"  :: [Int]
[1,2,3,4]
*Main> map read . words $ "1 2 3 4"  :: [Double]
[1.0,2.0,3.0,4.0]

read로 변수 읽었을 때 출력되지 않는 exception이 발생하는데, 이에 대해서는 반드시
어떤 형태로 출력할 것인지에 대한 내용을 전달해줘야 한다.
-}
