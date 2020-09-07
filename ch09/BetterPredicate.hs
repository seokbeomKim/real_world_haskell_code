{-# LANGUAGE ScopedTypeVariables #-}

-- file: ch09/BetterPredicate.hs
import           Control.Exception (bracket, handle)
import           Control.Monad
import           Data.Time
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtensions)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import           RecursiveContents (getRecursiveContents)

betterFind :: Predicate -> FilePath -> IO [FilePath]
-- simpleFileSize :: FilePath -> IO Integer
-- saferFileSize :: FilePath -> IO (Maybe Integer)

{- filterM의 타입은 다음과 같다.
*Main Control.Monad> :type filterM
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-}
betterFind p path = getRecursiveContents path >>= filterM check
 where
  check name = do
    perms    <- getPermissions name
    size     <- getFileSize name
    modified <- getModificationTime name
    return (p name perms size modified)

{- openFile의 경우 단순하게 Integer 만을 사용하여 처리해서는 안된다. plain
파일이 아닌 경우, Maybe 타입을 사용하여 디렉토리 등 해당되지 않는 파일에
대해서는 Nothing을 리턴하고 exception을 throw 하지 않도록 처리해줘야 한다.
하지만, 아래의 경우에도 문제가 될 수 있는데 O/S 에서 제한하는 열려있는 파일의
개수 제한 때문이다. 만약 openFile 부터 hClose 까지 GC 에 의해 정상적으로
처리된다면 모르겠지만 그렇지 않은 경우에는 문제가 될 수 있다.

또한, 반드시 hClose는 openFile이 성공할 때만 호출될 수 있도록 해야한다.

simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize path = handle (\_ -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

bracket 을 이용하여 호출한 함수에 대한 exception 핸들링을 try-catch 구문처럼
처리할 수 있다.

:type bracket bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

첫 번째 인자는 "resource"이다. 두 번째는 resource를 해제하는 부분, 세번째는
resource를 얻었을 때 어떻게 할 것인지, use 액션을 정의한다. 만일 use 액션이
실행되는 가운데 exception 이 발생했다면 bracket은 release 액션을 호출하고
예외를 다시 던진다.
-}

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
  handle (\_ -> return Nothing) $ bracket (openFile path ReadMode) hClose
    $ \h -> do
        size <- hFileSize h
        return (Just size)

-- Predicate의 리턴 값은 IO Bool 이 아닌 Bool이므로 pure 하다고 판단할 수 있다.
type Predicate
  =  FilePath           -- path to directory entry
  -> Permissions      -- permissions
  -> Maybe Integer    -- file size (Nothing if not file)
  -> UTCTime        -- last modified
  -> Bool

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a
-- returns one of Predicates arguments (extraction)
pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

{-
아래는 lifting을 설명하기 위한 것으로서 함수를 다른 함수 형태로
변환하는 것을 lifting이라 한다. 즉 리프링을 위한 helper 함수(lift
function)가 있고 이를 이용하여 원본 함수 (unlifted)와 변환된 함수
(lifted)함수를 얻는 식으로 활용한다.
-}
greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

listP' q f k w x y z = f w x y z `q` constP k w x y z

{- 하스켈에서, 함수를 인자로 전달받아 새로운 함수를 반환하는 함수들을
 일컫어 Combinator 라고 부른다.
-}
myTest path _ (Just size) _ = takeExtensions path == ".cpp" && size > 131072
myTest _ _ _ _              = False

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtensions `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

-- 아래와 갈이 새로운 연산자를 정의할 수 있다.
(==?) = equalP
(&&?) = andP
(>?) = greaterP
