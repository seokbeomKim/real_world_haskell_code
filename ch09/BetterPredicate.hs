-- file: ch09/BetterPredicate.hs
import           Control.Exception (bracket, handle)
import           Control.Monad
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtensions)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)
import           System.Time

-- the function we wrote earlier
import           RecursiveContents (getRecursiveContents)

type Predicate = FilePath
                -> Permissions
                -> Maybe Integer
                -> ClockTime
                -> Bool
