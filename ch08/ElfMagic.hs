-- file: ch08/ElfMagic.hs
-- 파일이 ELF object 파일인지 아닌지를 판단하는 간단한 예제이다.
-- 파일 종류를 식별하는 바이트 시퀀스는 magic number 라고 불리며, 이 코드에서는
-- magic number를 조사하고 결과에 따라 ELF object 인지를 판단한다.

-- qualified ... as x 는 x를 앞에 붙이고 ...에 정의된 내용을 사용하라는 뜻이
-- 아니라, Data.ByteString.Lazy가 L로써 qualified 되었다는 의미이다. 예를 들어,
-- foldr'은 unqualified name이며, Data.List.foldr' 이 qualified name이다.
-- 명시적으로 qualified 이름을 사용하겠다는 키워드이며, Shortname으로서 as
-- 키워드를 이용하여 명시적으로 namespace를 정의한다.
import qualified Data.ByteString.Lazy as L

-- ByteString 모듈은 binary I/O를 위해 만들어진 모듈이다. 예제에서는
-- bottom-up 방식으로 사고했지만, divide-in-conquer방식으로
-- top-down방식으로 내려가는 것이 프로그래밍하기에 훨씬 간편하다.

-- 예제와 같은 방식은 이미 L.ByteString이 어떤 것인지를 알고 있는
-- 상태여야 가능한 사고 방식이다.

main :: IO ()
main = do
  -- 예제는 아래와 같이 isElfFile 호출 하나만으로 ELF 포맷의 값을
  -- 함께 가져오지만, 문맥적으로 맞지 않는다.
  getLine >>= isElfFile >>= print -- >>= return 생략

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x54, 0x68, 0x69, 0x73]
    -- where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

hasElfMagicIO :: L.ByteString -> IO Bool
hasElfMagicIO content = do
  -- L.take :: Int -> [a] -> [a]

  -- take n, applied to a list xs, returns the prefix of xs of length n, or
  -- xs itself if n > length xs

  -- https://hackage.haskell.org/package/bytestring-0.11.0.0/docs/Data-ByteString-Lazy.html#t:ByteString
  -- According to the reference about ByteString type, it is described
  -- as following:
  -- A space-efficient representation of a Word8 vector, supporting
  -- many efficient operations.
  return $ L.take 4 content == elfMagic
  where elfMagic = L.pack [0x54, 0x68, 0x69, 0x73] -- To compare to content, [Word8]
                                                   -- each value has to be Word8

{-
Notes
~~~~~

- 각각의 함수들의 prototype
- print가 중간에 있어야 하는 이유는 무엇인가?
- isElfFile은 FilePath -> IO Bool인데, IO type 키워드를 이용해 alias
  형태로 정의되어 있기에 문제없이 로드된다.

type FilePath = String
getLine :: IO String
isElfFIle :: FilePath -> IO Bool
print :: Show a => a -> IO ()
return :: Monad m => a -> m a
-}

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    -- L.readFile은 lazy ByteString 함수이며, 파일 데이터를 요구할 때에만
    -- 읽기작업을 한다. 또한 최대 64 KB 크기 만큼 한번에 읽을 수 있고 현재처럼 4
    -- 바이트만 읽고자 하는 경우에 매우 효율적으로 동작한다.

    -- L.readFile: FilePath -> IO ByteString
    -- content <- L.readFile path
    -- return (hasElfMagic content)
  L.readFile path >>= hasElfMagicIO

{-
>>= (bind) 의 타입은 아래와 같다:
(>>=) :: Monad m => m a -> (a -> m b) -> m b

그러므로, 모나드 끼리의 bind를 위해서는 반드시 인자로 사용하는 함수의
반환값 또한 모나드 형태가 되어야 하며, action 끼리의 chaining 만
가능하다.

위 예제에서, binding 되는 함수들의 프로토타입은 다음과 같다. 즉, 모든
함수에 대해 반환값이 모나드 형태이다.

- readFile :: FilePath -> IO ByteString
- hasElfMagicIO :: L.ByteString -> IO Bool
- return :: Monad m => a -> m a
-}
