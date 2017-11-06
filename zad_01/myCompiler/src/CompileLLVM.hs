module Main where

import Data.List ( isSuffixOf )
import System.IO -- ( stdin, hGetContents, openFile )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexInstant
import ParInstant
import LLVMInstant
import AbsInstant

import ErrM


runFile :: FilePath -> IO ()
runFile f = do 
  s <- readFile f
  h <- getOutputFile f
  runCompiler h s


getOutputFile :: FilePath -> IO Handle
getOutputFile f = if ".ins" `isSuffixOf` f 
    then do
      let n = length f
      let nf = (take (n - 4) f) ++ ".ll"
      openFile nf WriteMode
    else
      return stdout


runCompiler :: Handle -> String -> IO ()
runCompiler h s = let ts = myLexer s in case pProgram ts of
  Ok tree -> do 
    let res = compile tree in            
      mapM_ (hPutStrLn h) res
    hClose h
    exitSuccess
  _ -> do
    putStrLn "\nParse failure"
    exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= runCompiler stdout
    fs -> mapM_ runFile fs
