module Main where

import Common (genericMain, getFileAndDirOrFail, syserr)
import qualified Instant.Llvm.Transpiler as Transpiler
import System.Exit (exitFailure, exitSuccess)

compileFile :: String -> IO ()
compileFile filename = do
    (dir, name) <- getFileAndDirOrFail filename

    contents <- readFile filename
    let maybeLlvmIRCode = Transpiler.run contents
    case maybeLlvmIRCode of
        Left msg -> syserr msg
        Right llvmIRCode -> do
            exitSuccess

-- TIO.writeFile jasmineFile jasmineCode
-- process <- runCommand jasmineCmd
-- exitCode <- waitForProcess process
-- unless (exitCode == ExitSuccess) (syserr $ "Jasmin failed with code: " ++ show exitCode)
-- exitSuccess

usage :: IO ()
usage = do
    putStrLn $
        unlines
            [ "Instant LLVM compiler."
            , "Usage: Call with one of the following argument combinations:"
            , "  --help         Display this help message."
            , "  (file)         TODO."
            ]
    exitFailure

main :: IO ()
main = genericMain compileFile usage (syserr "Invalid or no arguments provided")
