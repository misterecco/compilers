module CodeGen where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live

data CGReg = RDI | RSI | RDX | RCX | R8 | R9 | RAX | R10 | R11
            | RBX | R12 | R13 | R14 | R15
    deriving (Show, Eq, Ord)

data CGMem = Reg CGReg | Mem CGReg Integer

data CGMachineState = CGMS {
    regToVar :: Map CGReg IRAddr,
    varToMem :: Map IRAddr CGMem,
    nextInstrs :: [(IRInstr, LiveMap)],
    generatedCode :: [String]
}

data CGState = CGS {
    blocks :: Map Label LiveBlock,
    liveIn :: Map Label (Set IRAddr),
    strToLbl :: Map String Label,
    nextStrLbl :: Integer,
    currentMs :: CGMachineState,
    blockToMs :: Map Label CGMachineState,
    blockToInitialMs :: Map Label CGMachineState
}

type CGMonad = State CGState

initialMs :: LiveBlock -> CGMachineState
initialMs (LB _phi instrs _nb _pb) = CGMS M.empty M.empty instrs []

initialCGState :: LiveState -> [Label] -> CGState
initialCGState (LS _ lm bl) ord = do
    let initMss = M.fromList $ Prelude.map (\lbl -> (lbl, initialMs (bl ! lbl))) ord
    let initMainMs = initMss ! "main"
    CGS bl lm M.empty 0 initMainMs initMss M.empty

getBlock :: Label -> CGMonad LiveBlock
getBlock lbl = do
    CGS bls _ _ _ _ _ _ <- get
    return $ bls ! lbl

getLiveIn :: Label -> CGMonad (Set IRAddr)
getLiveIn lbl = do
    CGS _ lm _ _ _ _ _ <- get
    return $ lm ! lbl

freshStringLbl :: String -> CGMonad Label
freshStringLbl str = do
    CGS bl lm s2l nsl cms b2ms b2ims <- get
    let newLbl = "str_" ++ show nsl
    put $ CGS bl lm (M.insert str newLbl s2l) (nsl+1) cms b2ms b2ims
    return newLbl

getStringLbl :: String -> CGMonad Label
getStringLbl str = do
    CGS _ _ s2l _ _ _ _ <- get
    case M.lookup str s2l of
        Nothing -> freshStringLbl str
        Just lbl -> return lbl

setCurrentMs :: Label -> CGMonad ()
setCurrentMs lbl = do
    nextCurrentMs <- getMs lbl
    CGS lb lm s2l nsl _ b2ms b2ims <- get
    put $ CGS lb lm s2l nsl nextCurrentMs b2ms b2ims

getMs :: Label -> CGMonad CGMachineState
getMs lbl = do
    CGS _ _ _ _ _ b2ms _ <- get
    return $ b2ms ! lbl

setMs :: Label -> CGMachineState -> CGMonad ()
setMs lbl ms = do
    CGS lb lm s2l nsl cms b2ms b2ims <- get
    put $ CGS lb lm s2l nsl cms (M.insert lbl ms b2ms) b2ims

getInitMs :: Label -> CGMonad CGMachineState
getInitMs lbl = do
    CGS _ _ _ _ _ _ b2ims <- get
    return $ b2ims ! lbl

setInitMs :: Label -> CGMachineState -> CGMonad ()
setInitMs lbl ms = do
    CGS lb lm s2l nsl cms b2ms b2ims <- get
    put $ CGS lb lm s2l nsl cms b2ms (M.insert lbl ms b2ims)



generateAsm :: LiveState -> [Label] -> [String]
generateAsm (LS _ li extb) blockOrder = do
    ["hello"]
