-- myWork.hs: 예제 파일로부터 직접 ghci에서 테스트하기 위한 파일

import Data.Bool
import System.Environment (getArgs)
import System.Exit

main = do
  args <- getArgs
  print $ "Input: " ++ unwords args ++ ", length: " ++ (show $ length args)
  print $ "First: " ++ args !! 0

  content <- readFile (args !! 0)
  let perm = getPermissionByName content "lookup.hs"

  print $ "Permission of lookup.hs: "
  print $ perm

-- 예제에서는 /etc/passwd 를 가지고 진행했었다.  이를 응용하여, 현재
-- 디렉토리의 파일들의 권한을 검사하여 readable인지, writable인지,
-- executable인지 판단할 수 있는 간단한 함수를 만들어보자. 데이터는
-- 미리 파일 형태로 준비한다.

type Readable = Bool
type Writable = Bool
type Executable = Bool

newtype Permission = Permission (Readable, Writable, Executable)
                     deriving (Eq)

instance Show Permission where
  show (Permission (r,w,x)) = "Readable: " ++ (show r) ++
                              ", Writable: " ++ (show w) ++
                              ", Executable: " ++ (show x)

getPermissionByName :: String -> String -> Maybe Permission
getPermissionByName content name =
  let a1 = map parseLine (lines $ content)
  in lookup name a1

parseLine :: String -> (String, Permission)
parseLine input =
  let fields = words input
   in ((fields !! 8), permissionFromString $ fst (splitAt 3 $ fields !! 0))

permissionFromString :: String -> Permission
permissionFromString str =
  let readable = 'r' `elem` str
      writable = 'w' `elem` str
      executable = 'x' `elem` str
   in Permission (readable, writable, executable)
