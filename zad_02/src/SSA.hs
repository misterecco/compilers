module SSA where

import IRDef
import CFG ( CFGBlock, CFGState(..) )

import Prelude hiding ( lookup )

import Control.Monad.State
import Data.Map hiding ( map )


data SSAState = SSAS {
    blockOrder :: [Label],
    blocks :: Map Label CFGBlock,
    varMapping :: Map (Label, IRAddr) IRAddr,
    visitedBlocks :: [Label],
    nextVal :: Integer
}

type SSAMonad = State SSAState

initialSSAState :: CFGState -> SSAState
initialSSAState (CFGS bo bl) = SSAS bo bl empty [] 0

getBlockOrder :: SSAMonad [Label]
getBlockOrder = do
    SSAS bo _ _ _ _ <- get
    return bo

setBlock :: Label -> CFGBlock -> SSAMonad ()
setBlock lbl bl = do
    SSAS bo bs vm vb nv <- get
    put $ SSAS bo (insert lbl bl bs) vm vb nv

getBlock :: Label -> SSAMonad CFGBlock
getBlock lbl = do
    SSAS _ bls _ _ _ <- get
    return $ bls ! lbl

setMapping :: (Label, IRAddr) -> IRAddr -> SSAMonad ()
setMapping v val = do
    SSAS bo bl vm vb nv <- get
    put $ SSAS bo bl (insert v val vm) vb nv

getMapping :: (Label, IRAddr) -> SSAMonad (Maybe IRAddr)
getMapping v = do
    SSAS _ _ vm _ _ <- get
    return $ lookup v vm

isVisited :: Label -> SSAMonad Bool
isVisited lbl = do
    SSAS _ _ _ vb _ <- get
    return $ lbl `elem` vb

addVisited :: Label -> SSAMonad ()
addVisited lbl = do
    SSAS bo bl vm vb nv <- get
    put $ SSAS bo bl vm (lbl:vb) nv

freshVal :: IRType -> SSAMonad IRAddr
freshVal t =
    if t == IRVoid then 
        return NoRet else do
            SSAS bo bl vm vb nv <- get
            put $ SSAS bo bl vm vb (nv+1)
            let name = "val-" ++ show nv
            return $ Indirect (IRVar name 0 t)
