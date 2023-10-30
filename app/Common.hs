module Common where

import System.Directory.Internal.Prelude (exitFailure)
import System.Environment (getArgs)
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