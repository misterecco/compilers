module Main where
  
import Data.List ( isSuffixOf )
import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Process
import Control.Monad ( when )

import LexLatte
import ParLatte
import AsmLatte
import AbsLatte
import PrintLatte
import Preprocessor
import Generator

import ErrM


showTree :: (Show a, Print a) => a -> IO ()
showTree tree =
  -- hPutStrLn stderr $ "\n[Abstract Syntax]\n\n" ++ show tree
  hPutStrLn stderr $ "\n[Linearized tree]\n\n" ++ printTree tree


runFile :: FilePath -> IO ()
runFile f = do 
  s <- readFile f
  let path = getTestOutputPath f
  runCompiler path s
  -- runASM path


runASM :: FilePath -> IO ()
runASM path = do
  let binPath = getBinaryOutputPath path
  callCommand $ "gas -o " ++ binPath ++ " " ++ path


getTestOutputPath :: FilePath -> FilePath
getTestOutputPath f = 
  if ".lat" `isSuffixOf` f then
    let n = length f in
    take (n - 4) f ++ ".s"
  else "out.s"


getBinaryOutputPath :: FilePath -> FilePath
getBinaryOutputPath f = 
  let n = length f in
    take (n - 3) f


printUsage :: IO ()
printUsage =
  mapM_ putStrLn [ "latc_x86_64 <path_to_input_file>"
                  , "   note: input file should be located in a subdirectory"
                  , "   and have an extension .lat" ]


runCompiler :: FilePath -> String -> IO ()
runCompiler path s = let ts = myLexer s in case pProgram ts of
  Ok tree -> do 
    let nt = preprocess tree
    case nt of
      Left e -> do
        hPutStrLn stderr "ERROR"    
        hPutStrLn stderr e
        exitFailure
      Right tr -> do 
        hPutStrLn stderr "OK"
        showTree tr 
        let instrs = generateIR tr
        mapM_ (hPrint stderr) instrs
        exitSuccess
    -- h <- openFile path WriteMode    
    -- mapM_ (hPutStrLn h) (compile tree)
    -- hClose h
  Bad e -> do
    hPutStrLn stderr "ERROR"
    hPutStrLn stderr e
    exitFailure


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage    
    [fs] -> runFile fs
    _ -> printUsage
