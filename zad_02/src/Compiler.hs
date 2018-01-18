module Main where
  
import Control.Monad
import Data.List ( isSuffixOf )
import Data.Map ( (!) )
import System.IO
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import ParLatte
import PrintLatte
import Preprocessor
import IRGen
import CFG
import SSA
import Live
import IRDef ( Label )
import CodeGen

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
    runASM path


runASM :: FilePath -> IO ()
runASM path = do
    let binPath = getBinaryOutputPath path
    let command = "gcc -no-pie -o " ++ binPath ++ " " ++ path ++ " ../lib/builtins.s"
    hPutStrLn stderr command
    callCommand $ command
    exitSuccess


getTestOutputPath :: FilePath -> FilePath
getTestOutputPath f = 
    if ".lat" `isSuffixOf` f then
        let n = length f in
        take (n - 4) f ++ ".s"
    else "out.s"


getBinaryOutputPath :: FilePath -> FilePath
getBinaryOutputPath f = 
    let n = length f in
        take (n - 2) f


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
        Left e -> exitWithError e
        Right tr -> do 
            hPutStrLn stderr "OK"
            showTree tr 
            let instrs = generateIR tr
            -- mapM_ (hPrint stderr) instrs
            let cfgs = generateCFG instrs
            printCFGState cfgs
            let ssas@(CFGS ord _) = convertToSSA cfgs
            printCFGState ssas
            let igs = calculateLiveliness ssas
            printLiveState ord igs
            let asmCode = generateAsm igs ord
            h <- openFile path WriteMode    
            mapM_ (hPrint h) asmCode 
            hClose h            
  Bad e -> exitWithError e
  where 
    exitWithError er = do
      hPutStrLn stderr "ERROR"    
      hPutStrLn stderr er
      exitFailure


printCFGState :: CFGState -> IO ()
printCFGState (CFGS ord bls) = do
    hPrint stderr ord
    hPutStrLn stderr ""
    forM_ ord (\lbl -> do
        let bl = bls ! lbl
        hPutStrLn stderr $ "Block: " ++ lbl
        hPrint stderr bl )


printLiveState :: [Label] -> LiveState -> IO ()
printLiveState ord (LS bls ins ebls) = do
    hPrint stderr ord
    hPutStrLn stderr ""
    forM_ ord (\lbl -> do
        let inSet = ins ! lbl
        let extBlock = bls ! lbl
        hPutStrLn stderr $ "Block: " ++ lbl
        hPutStrLn stderr "In set:"
        hPrint stderr inSet 
        hPutStrLn stderr "Instructions:"
        hPrint stderr extBlock        
        hPutStrLn stderr "")


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> printUsage    
    [fs] -> runFile fs
    _ -> printUsage
