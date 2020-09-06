-- file: ch09/ControlledVisit.hs

import           Control.Exception (bracket, handle)
import           Control.Monad
import           Data.Time
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtensions)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

data Info = Info {
    infoPath      :: FilePath
    , infoPerms   :: Maybe Permissions
    , infoSize    :: Maybe Integer
    , infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info

{- 파일이나 디렉토리에 대한 정보를 얻기 위해 getInfo action을 이용한다. -}
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)

    {- Lifting: https://wiki.haskell.org/Lifting -}
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
