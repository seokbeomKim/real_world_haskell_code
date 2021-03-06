-- file: ch07/tempfile.hs

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finally, catch)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

{- The guts of the program. Called with the path and handle of a temporary file.
When this function exits, that file will be closed and deleted because myAction
was called from withTempFile. -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- start by displaying a greeting on the terminal
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname

        -- Let's see what the initial position is
        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        -- write some data to the temporary file
        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++ 
                    show (length tempdata) ++ " bytes: " ++ tempdata
        hPutStrLn temph tempdata

        pos <- hTell temph
        putStrLn $ "After writing, the position is " ++ show pos

        -- seek to the beginning of the file and display it
        putStrLn $ "The file content is: "
        hSeek temph AbsoluteSeek 0

        c <- hGetContents temph

        putStrLn c

        putStrLn $ "Which could be expressed as this Haskell literal: "
        print c

{- This function takes two parameters: a filename pattern and another function.
 It will create a temporary file, and pass the name and Handle of that file to
 the given function. 

 The temporary file is created with openTempFile. The directory is the one
 indicated by getTemporaryDirectory, or, if the system has no notion of a
 temporary directory, "." is used. The given pattern is passed to
 openTempFile. -}

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
   tempdir <- getTemporaryDirectory
   (tempfile, temph) <- openTempFile tempdir pattern
   finally (func tempfile temph)
           (do hClose temph 
               removeFile tempfile)

