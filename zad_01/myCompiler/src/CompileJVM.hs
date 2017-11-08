module Main where

import Data.List ( isSuffixOf )
import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Process
import Control.Monad ( when )

import LexInstant
import ParInstant
import JVMInstant
import AbsInstant

import ErrM


runFile :: FilePath -> IO ()
runFile f = do 
  s <- readFile f
  let path = getTestOutputPath f
  runCompiler path s
  runJVM path


runJVM :: FilePath -> IO ()
runJVM path = do
  let dir = getDirOutputPath path
  callCommand $ "java -jar lib/jasmin.jar -d " ++ dir ++ " " ++ path


getTestOutputPath :: FilePath -> FilePath
getTestOutputPath f = 
  if ".ins" `isSuffixOf` f then
    let n = length f in
      (take (n - 4) f) ++ ".j"
  else "out.j"


getDirOutputPath :: FilePath -> FilePath
getDirOutputPath f = reverse $ dropWhile (/= '/') $ reverse f


getClassName :: FilePath -> String
getClassName f = reverse $ takeWhile (/= '/') $ reverse ff
  where
    n = length f
    ff = take (n - 2) f


printUsage :: IO ()
printUsage = do
  mapM_ putStrLn [ "insc_jvm <path_to_input_file>"
                 , "   note: input file should be located in a subdirectory"
                 , "   and have an extension .ins" ]


runCompiler :: FilePath -> String -> IO ()
runCompiler path s = let ts = myLexer s in case pProgram ts of
  Ok tree -> do 
    h <- openFile path WriteMode
    let c = getClassName path
    mapM_ (hPutStrLn h) (compile c tree)
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
