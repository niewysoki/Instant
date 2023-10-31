module Main where

import Common (genericMain, getFileAndDirOrFail, processCodeWithCommand, syserr)
import qualified Instant.Jvm.Transpiler as Transpiler
import System.FilePath (replaceExtension)

transpileFile :: String -> IO ()
transpileFile filename = do
    (dir, name) <- getFileAndDirOrFail filename
    let jasmineFile = replaceExtension filename ".j"
        jasmineCmd = "java -jar ./lib/jasmin.jar -d " ++ dir ++ " " ++ jasmineFile
    contents <- readFile filename
    let mbCode = Transpiler.run name contents
    processCodeWithCommand mbCode jasmineFile jasmineCmd (syserr . ("Jasmin failed with code: " ++) . show)

main :: IO ()
main = genericMain transpileFile usage
  where
    usage =
        unlines
            [ "Instant JVM transpiler."
            , "Usage: Call with one of the following argument:"
            , "  --help         Display this help message."
            , "  (file)         Transpile content of the file into .j and .class files in the same directory."
            ]
