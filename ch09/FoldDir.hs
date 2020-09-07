-- file: ch09/FoldDir.hs

import           Control.Exception (bracket, handle)
import           Control.Monad
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtensions)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)
import           System.Time

data Info = Info {
    infoPath      :: FilePath
    , infoPerms   :: Maybe Permissions
    , infoSize    :: Maybe Integer
    , infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

data Iterate seed = Done    { unwrap :: seed }
                  | Skip    { unwrap :: seed }
                  | Continue{ unwrap :: seed }
                  deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

{- ControlledVisit의 문제점은 읽고자 하는 디렉토리에 대한 탐색이 모두 끝날
때까지 기다려야 한다는 점이다. 이를 해결하기 위해서 탐색에 대한 상태값을 담을 수
있는 data들을 위와 같이 정의하고 아래와 같이 각 상태에 따른 행동을 정의한다.

* 명령이 Done 인 경우, 탐색을 즉시 중단해야 한다.
* 명령이 Skip 인 경우 현재 디렉토리를 나타내는 Info 타입을 탐색하지 않도록 한다.
* 다른 경우에는 탐색을 계속 진행한다.
-}

-- foldl 을 이용하여 결과 값을 쌓아가는 방식으로 구현한다.
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
        endSeed <- fold initSeed path
        return (unwrap endSeed)
    where
        fold seed subpath = getUsefulContents subpath >>= walk seed

        walk seed (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _) -> return done
                Skip seed' -> walk seed' names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold seed' path'
                        case next of
                            done@(Done _) -> return done
                            seed''        -> walk (unwrap seed'') names
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)
