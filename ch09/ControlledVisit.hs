-- file: ch09/ControlledVisit.hs

import           Control.Exception (bracket, handle)
import           Control.Monad
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtensions)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)
import           System.Time

{- type constructor와 data constructor를 아래와 같이 정의하였다. type
constructor는 여러 개의 data constructor로 구성될 수 있으며, 아래 타입은 한 개의
data constructor로만 구성되어 있다. Info data structor는 인자 4개를 가지며, Eq,
Ord, Show typeclass를 상속하고 있다.
-}
data Info = Info {
    infoPath      :: FilePath
    , infoPerms   :: Maybe Permissions
    , infoSize    :: Maybe Integer
    , infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `listM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)

{- 파일이나 디렉토리에 대한 정보를 얻기 위해 getInfo action을 이용한다. -}
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)

    {- Lifting: https://wiki.haskell.org/Lifting 아래 라인에서 liftM은 함수를
    전달받는데. 즉, forM으로부터 (type IO [[Info]]를 전달받고 이를 concat 하여
    IO monad로 return한다.) -}
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
