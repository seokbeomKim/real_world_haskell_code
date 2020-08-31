-- file: ch09/RecursiveContents.hs
module RecursiveContents (getRecursiveContents) where

import           Control.Monad    (forM)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
    names <- getDirectoryContents topdir

    -- filter은 이름 중에서 리스트들에 포함되지 않은 것을 필터링한다.
    let properNames = filter (`notElem` [".", ".."]) names

    -- forM은 mapM의 파라미터 구조의 반대로서 아래와 같다.
    -- forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)

{-
mapM vs. forM
루프가 짧고 데이터가 길다면 mapM 사용
루프가 길고 데이터가 짧다면 forM 사용
-}
