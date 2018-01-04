module Generator where

import AbsLatte
import CompState
import PreState ( Position )

import Data.Map hiding (map, foldr)
import Data.List hiding (insert)
import Control.Monad.State

convertArgToVar :: Arg Position -> CompMonad Var
convertArgToVar (Arg _ _ (Ident var)) = return var

convertArgsToVars :: [Arg Position] -> CompMonad [Var]
convertArgsToVars = mapM convertArgToVar

-- toIrStmts :: [Stmt Position] -> CompMonad [IRInstr]

toIrFunction :: Block Position -> [Arg Position] -> CompMonad IRBlock
toIrFunction (Block _ stmts) args = do
    vars <- convertArgsToVars args
    return $ B [] vars [] []

toIrFunctions :: [TopDef Position] -> CompMonad ()
toIrFunctions ((FnDef _ _ (Ident label) args block):fns) = do
    b <- toIrFunction block args
    addBlock label b
    toIrFunctions fns
toIrFunctions [] = return ()

toIrProgram :: Program Position -> CompMonad ()
toIrProgram (Program _ topDefs) = toIrFunctions topDefs


generateBlocks :: Program Position -> Map Label IRBlock
generateBlocks p = blocks (execState (toIrProgram p) initialState)
