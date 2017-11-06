module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexInstant
import ParInstant
import LLVMInstant
import PrintInstant
import AbsInstant

import ErrM


runFile :: FilePath -> IO ()
runFile f = readFile f >>= runCompiler


runCompiler :: String -> IO ()
runCompiler s = let ts = myLexer s in case pProgram ts of
  Ok tree -> do 
    let res = compile tree in            
      mapM_ putStrLn res
    exitSuccess
  _ -> do
    putStrLn "\nParse failure"
    exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= runCompiler
    fs -> mapM_ runFile fs
