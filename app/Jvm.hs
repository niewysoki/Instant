{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Instant.Grammar.ErrM (Err, pattern Bad, pattern Ok)
import Instant.Grammar.ParInstant (myLexer, pProgram)
import Instant.Jvm.Transpiler (transpileProgramToJasmin)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitFailure, exitSuccess)
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)
import System.Process (runCommand, waitForProcess)

run :: String -> String -> IO Text
run name text = case pProgram . myLexer $ text of
    Bad msg -> fail $ "PARSER ERROR: " ++ msg
    Ok ast -> case transpileProgramToJasmin name ast of
        Bad msg -> fail $ "COMPILATION ERROR: " ++ msg
        Ok code -> return code

compileFile :: String -> IO ()
compileFile filename = do
    fileExists <- doesFileExist filename
    unless fileExists (fail $ "File not found: '" ++ filename ++ "'")

    let name = dropExtension $ takeFileName filename

    contents <- readFile filename
    jasmineCode <- run name contents

    let dir = takeDirectory filename
    let jasmineFile = replaceExtension filename ".j"

    TIO.writeFile jasmineFile jasmineCode

    let jasmineCmd = "java -jar ./lib/jasmin.jar -d " ++ dir ++ " " ++ jasmineFile
    process <- runCommand jasmineCmd
    code <- waitForProcess process

    unless (code == ExitSuccess) (fail $ "Jasmin failed with code: " ++ show code)
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
main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        [f] -> compileFile f
        _ -> exitFailure