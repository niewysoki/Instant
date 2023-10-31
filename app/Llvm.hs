module Main where

import Common (genericMain, getFileAndDirOrFail, processCodeWithCommand, syserr)
import qualified Instant.Llvm.Transpiler as Transpiler
import System.FilePath (replaceExtension)

transpileFile :: String -> IO ()
transpileFile filename = do
    _ <- getFileAndDirOrFail filename
    let llFile = replaceExtension filename ".ll"
        bcFile = replaceExtension filename ".bc"
        llvmCommand = "llvm-as -o " ++ bcFile ++ " " ++ llFile
    contents <- readFile filename
    let mbCode = Transpiler.run contents
    processCodeWithCommand mbCode llFile llvmCommand (syserr . ("llvm-as failed with code: " ++) . show)

main :: IO ()
main = genericMain transpileFile usage
  where
    usage =
        unlines
            [ "Instant to LLVM transpiler."
            , "Usage: Call with one of the following argument:"
            , "  --help         Display this help message."
            , "  (file)         Transpile content of the input file into .ll and .bc files in the same directory."
            ]
