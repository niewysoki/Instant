module Main where

import Common (genericMain, getFileAndDirOrFail, processCodeWithCommand, syserr)
import qualified Instant.Jvm.Transpiler as Transpiler
import System.FilePath (replaceExtension)

compileFile :: String -> IO ()
compileFile filename = do
    (dir, name) <- getFileAndDirOrFail filename
    let jasmineFile = replaceExtension filename ".j"
        jasmineCmd = "java -jar ./lib/jasmin.jar -d " ++ dir ++ " " ++ jasmineFile
    contents <- readFile filename
    let mbCode = Transpiler.run name contents
    processCodeWithCommand mbCode jasmineFile jasmineCmd (syserr . ("Jasmin failed with code: " ++) . show)

usage :: String
usage =
    unlines
        [ "Instant JVM compiler."
        , "Usage: Call with one of the following argument combinations:"
        , "  --help         Display this help message."
        , "  (file)         Compile content of the file into .j and .class files in the file's directory."
        ]

main :: IO ()
main = genericMain compileFile usage (syserr "Invalid or no arguments provided")
