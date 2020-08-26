-- file: ch08/ElfMagic.hs
-- 파일 종류를 식별하는 바이트 시퀀스는 magic number 라고 불린다.

import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic 
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do 
    content <- L.readFile path
    return (hasElfMagic content)    

main = do 
    inpFile <- getLine
    print $ "input file : " ++ inpFile
    rvalue <- isElfFile inpFile
    print $ rvalue