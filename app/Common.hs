module Common where

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)

genericMain :: (String -> IO b) -> IO b -> IO b -> IO b
genericMain onFile onHelp onErr = do
    args <- getArgs
    case args of
        ["--help"] -> onHelp
        [f] -> onFile f
        _ -> onErr

syserr :: String -> IO ()
syserr msg = do
    hPutStrLn stderr msg
    exitFailure

getFileAndDirOrFail :: String -> IO (String, String)
getFileAndDirOrFail filename = do
    fileExists <- doesFileExist filename
    unless fileExists (syserr $ "File not found: '" ++ filename ++ "'")

    let name = dropExtension $ takeFileName filename
    let dir = takeDirectory filename
    return (dir, name)