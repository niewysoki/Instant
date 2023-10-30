module Main where

import Common (genericMain, getFileAndDirOrFail, syserr)
import Control.Monad (unless)
import qualified Data.Text.IO as TIO
import qualified Instant.Jvm.Transpiler as Transpiler
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)
import System.FilePath (replaceExtension)
import System.Process (runCommand, waitForProcess)

compileFile :: String -> IO ()
compileFile filename = do
    (dir, name) <- getFileAndDirOrFail filename
    let jasmineFile = replaceExtension filename ".j"
    let jasmineCmd = "java -jar ./lib/jasmin.jar -d " ++ dir ++ " " ++ jasmineFile

    contents <- readFile filename
    let maybeJasmineCode = Transpiler.run name contents
    case maybeJasmineCode of
        Left msg -> syserr msg
        Right jasmineCode -> do
            TIO.writeFile jasmineFile jasmineCode
            process <- runCommand jasmineCmd
            exitCode <- waitForProcess process
            unless (exitCode == ExitSuccess) (syserr $ "Jasmin failed with code: " ++ show exitCode)
            exitSuccess

usage :: IO ()
usage = do
    putStrLn $
        unlines
            [ "Instant JVM compiler."
            , "Usage: Call with one of the following argument combinations:"
            , "  --help         Display this help message."
            , "  (file)         Compile content of the file into .j and .class files in the file's directory."
            ]
    exitFailure

main :: IO ()
main = genericMain compileFile usage (syserr "Invalid or no arguments provided")
