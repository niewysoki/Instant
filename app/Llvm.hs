module Main where

import Common (genericMain, getFileAndDirOrFail, processCodeWithCommand, syserr)
import qualified Instant.Llvm.Transpiler as Transpiler
import System.FilePath (replaceExtension)

compileFile :: String -> IO ()
compileFile filename = do
    _ <- getFileAndDirOrFail filename
    let llFile = replaceExtension filename ".ll"
        bcFile = replaceExtension filename ".bc"
        llvmCommand = "llvm-as -o " ++ bcFile ++ " " ++ llFile
    contents <- readFile filename
    let mbCode = Transpiler.run contents
    processCodeWithCommand mbCode llFile llvmCommand (syserr . ("llvm-as failed with code: " ++) . show)

usage :: String
usage =
    unlines
        [ "Instant LLVM compiler."
        , "Usage: Call with one of the following argument combinations:"
        , "  --help         Display this help message."
        , "  (file)         Compile content of the input file into .ll and .bc files in the same directory."
        ]

main :: IO ()
main = genericMain compileFile usage (syserr "Invalid or no arguments provided")
