{- traverse 함수에 대해 하스켈에서의 density & readability가 중요한지를 나타내기
위한 verbose 버전 함수
-}

traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
    where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
            Nothing -> False
            Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]