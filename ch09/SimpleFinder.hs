-- file: ch09/SimpleFinder.hs
import           RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)

{- 위의 simpleFind의 경우 아래와 같은 문제가 있다.
1. 파일과 디렉토리를 구분하지 못한다.
2. 파일시스템을 어떻게 탐색할 것인지 컨트롤하지 못한다.
3. simpleFind는 strict 이므로 IO Monad에서 lazy stream을 이용하여
구현할 수 있어야 반응성이 좋아진다.
-}
