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

-- ByteString 모듈은 binary I/O를 위해 만들어진 모듈이다.

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x54, 0x68, 0x69, 0x73]
    -- where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    -- L.readFile은 lazy ByteString 함수이며, 파일 데이터를 요구할 때에만
    -- 읽기작업을 한다. 또한 최대 64 KB 크기 만큼 한번에 읽을 수 있고 현재처럼 4
    -- 바이트만 읽고자 하는 경우에 매우 효율적으로 동작한다.
    content <- L.readFile path
    return (hasElfMagic content)

main :: IO ()
main = do
    inpFile <- getLine
    print $ "input file : " ++ inpFile
    rvalue <- isElfFile inpFile
    print $ rvalue
