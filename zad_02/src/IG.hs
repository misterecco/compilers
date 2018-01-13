module IG where

import IRDef
import CFG ( CFGBlock(..), CFGState(..) )

import Control.Monad.State
import Data.Map as M hiding ( foldr )
import Data.Set as S hiding ( foldr )

data LiveState = LS {
    blocks :: Map Label CFGBlock,
    liveIn :: Map Label (Set IRAddr)
}

type LiveMonad = State LiveState

initialLiveState :: CFGState -> LiveState
initialLiveState (CFGS bo bl) = do
    let emptySetList = S.empty:emptySetList
    let emptyLives = zip bo emptySetList
    LS bl (M.fromList emptyLives)

getBlock :: Label -> LiveMonad CFGBlock
getBlock lbl = do
    LS bls _ <- get
    return $ bls ! lbl

getLiveMap :: LiveMonad (Map Label (Set IRAddr))
getLiveMap = do
    LS _ lm <- get
    return lm

getLiveIn :: Label -> LiveMonad (Set IRAddr)
getLiveIn lbl = do
    lm <- getLiveMap
    return $ lm ! lbl

setLiveIn :: Label -> Set IRAddr -> LiveMonad ()
setLiveIn lbl lives = do
    LS bl lm <- get
    put $ LS bl (M.insert lbl lives lm)


addAddr :: IRAddr -> Set IRAddr -> LiveMonad (Set IRAddr)
addAddr addr set = case addr of
    (Indirect _) -> return $ S.insert addr set
    _ -> return set

remAddr :: IRAddr -> Set IRAddr -> LiveMonad (Set IRAddr)
remAddr addr set = case addr of
    (Indirect _) -> return $ S.delete addr set
    _ -> return set


calcInstr :: IRInstr -> Set IRAddr -> LiveMonad (Set IRAddr)
calcInstr instr outs = case instr of
    IRAss _ dst larg rarg -> do
        o1 <- addAddr larg outs
        o2 <- addAddr rarg o1
        remAddr dst o2
    IRSAss _ dst arg -> do
        o1 <- addAddr arg outs
        remAddr dst o1
    IRCall dst _ args -> do
        o1 <- foldM (\o ar -> addAddr ar o) outs args
        remAddr dst o1
    IRIf _ larg rarg _ _ -> do
        o1 <- addAddr larg outs
        addAddr rarg o1
    IRGoto _ -> return outs
    IRCpy dst arg -> do
        o1 <- addAddr arg outs
        remAddr dst o1
    IRLabel _ -> return outs
    IRRet arg -> addAddr arg outs
    IRParam arg -> addAddr arg outs

calcInstrs :: [IRInstr] -> Set IRAddr -> LiveMonad (Set IRAddr)
calcInstrs [] ins = return ins
calcInstrs (i:is) outs = do
    ins <- calcInstr i outs 
    calcInstrs is ins

calcBlock :: Label -> LiveMonad ()
calcBlock lbl = do
    B _ instrs nb _ <- getBlock lbl
    nextIns <- mapM getLiveIn nb
    let outs = foldr S.union S.empty nextIns
    ins <- calcInstrs (reverse instrs) outs
    setLiveIn lbl ins


calcAllBlocks :: [Label] -> LiveMonad ()
calcAllBlocks bls = do
    preLm <- getLiveMap
    mapM_ calcBlock bls
    postLm <- getLiveMap
    unless (postLm == preLm) $ calcAllBlocks bls

calculateLiveliness :: CFGState -> LiveState
calculateLiveliness st@(CFGS bo _) = execState (calcAllBlocks (reverse bo)) (initialLiveState st)
