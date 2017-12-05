module Preprocessor where

import AbsLatte
import PreState

import Control.Monad.Except
import Control.Monad.State


type AnalysisResult = Either String (Program Position)





collectFunctions :: [TopDef Position] -> PreprocessorMonad [TopDef Position]
collectFunctions = mapM collectFunction

collectFunction :: TopDef Position -> PreprocessorMonad (TopDef Position)
collectFunction def = do
    addFunction def
    return def


analyzeProgram :: Program Position -> PreprocessorMonad (Program Position)
analyzeProgram (Program pos topDefs) = do
    tr <- collectFunctions topDefs
    return $ Program pos tr


preprocess :: Program Position -> AnalysisResult
preprocess p = evalState (runExceptT (analyzeProgram p)) emptyEnv
