module Main where

import Data.List ( isSuffixOf )
import System.IO -- ( stdin, hGetContents, openFile )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Process
import Control.Monad ( when )

import LexInstant
import ParInstant
import LLVMInstant
import AbsInstant

import ErrM


runFile :: FilePath -> IO ()
runFile f = do 
  s <- readFile f
  let path = getTestOutputPath f
  runCompiler path s
  runLLVM path


runLLVM :: FilePath -> IO ()
runLLVM path = do
  let binPath = getBinaryOutputPath path
  callCommand $ "llvm-as -o " ++ binPath ++ " " ++ path


getTestOutputPath :: FilePath -> FilePath
getTestOutputPath f = if ".ins" `isSuffixOf` f 
    then
      let n = length f in
      (take (n - 4) f) ++ ".ll"
    else "out.ll"


getBinaryOutputPath :: FilePath -> FilePath
getBinaryOutputPath f = 
  let n = length f in
    (take (n - 3) f) ++ ".bc"


printUsage :: IO ()
printUsage = do
  putStrLn "One file at a time please"


runCompiler :: FilePath -> String -> IO ()
runCompiler path s = let ts = myLexer s in case pProgram ts of
  Ok tree -> do 
    h <- openFile path WriteMode    
    mapM_ (hPutStrLn h) (compile tree)
    hClose h
  _ -> do
    putStrLn "\nParse failure"
    exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fs] -> runFile fs
    _ -> printUsage
