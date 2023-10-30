module Common where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Instant.Grammar.ErrM (Err)
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitSuccess)
import System.FilePath (dropExtension, takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (runCommand, waitForProcess)

genericMain :: (String -> IO ()) -> String -> IO ()
genericMain onFile usage = do
    args <- getArgs
    case args of
        ["--help"] -> putStrLn usage >> exitFailure
        [f] -> onFile f
        _ -> syserr "Invalid or no arguments provided"

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

processCodeWithCommand :: Err Text -> FilePath -> String -> (ExitCode -> IO ()) -> IO ()
processCodeWithCommand (Left msg) _ _ _ = syserr msg
processCodeWithCommand (Right code) outfile command onCommandFailure = do
    TIO.writeFile outfile code
    process <- runCommand command
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) (onCommandFailure exitCode)
    exitSuccess
