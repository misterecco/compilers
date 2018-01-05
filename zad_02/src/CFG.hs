module CFG where

import IRDef

import Control.Monad.State
import Data.List ( intercalate )
import Data.Map hiding ( map )


data CFGBlock = B {
    phi :: Map IRAddr IRAddr,
    instructions :: [IRInstr],
    nextBlocks :: [Label],
    prevBlocks :: [Label]
}

instance Show CFGBlock where
    show (B _phi instrs nb pb) = unlines 
        [ "Before: " ++ intercalate ", " pb
        , "After: " ++ intercalate ", " nb
        , "Instructions: "
        , unlines (map show instrs) ]

data CFGState = CFGS {
    blockOrder :: [Label],
    blocks :: Map Label CFGBlock
}

type CFGMonad = State CFGState

initialState :: CFGState
initialState = CFGS [] empty

addBlock :: Label -> [IRInstr] -> CFGMonad ()
addBlock label instrs = do
    CFGS bo bs <- get
    put $ CFGS bo (insert label (B empty instrs [] []) bs)

setBlockOrder :: [Label] -> CFGMonad ()
setBlockOrder bo = do
    CFGS _ bs <- get
    put $ CFGS bo bs


genHelper :: [IRInstr] -> [IRInstr] -> [Label] -> CFGMonad ()
genHelper [] [] bo = do
    let ordering = reverse bo
    setBlockOrder ordering
genHelper [] cb bo = do
    let instrs = reverse cb
    let (IRLabel lbl) = head instrs
    addBlock lbl instrs
    genHelper [] [] bo
genHelper (i:is) cb bo = case i of
    IRLabel l -> do
        when (length cb > 0) $ do
            let instrs = reverse cb
            let (IRLabel lbl) = head instrs
            addBlock lbl instrs
        genHelper is [i] (l:bo)
    _ -> genHelper is (i:cb) bo

generateCFG :: [IRInstr] -> CFGState
generateCFG instrs = execState (genHelper instrs [] []) initialState
