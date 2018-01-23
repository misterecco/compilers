module CGState where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import CGDef
import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )


registerPool :: Set CGReg
registerPool = S.fromList [R11, RBX, R12, R13, R14, R15, RDI, RSI, RCX, R8, R9]

nonvolatileRegisters :: [CGReg]
nonvolatileRegisters = [RBX, R12, R13, R14, R15]

volatileRegisters :: [CGReg]
volatileRegisters = [RAX, RDI, RSI, RCX, RDX, R8, R9, R10, R11]

paramRegisters :: [CGReg]
paramRegisters = [RDI, RSI, RDX, RCX, R8, R9]


initialCGState :: LiveState -> [Label] -> CGState
initialCGState (LS _ lm bl) ord = do
    let initMss = M.fromList $ Prelude.map (\lbl -> (lbl, initialMs (bl ! lbl))) ord
    let initMainMs = initMss ! "main"
    CGS bl lm M.empty 0 0 initMainMs initMss M.empty (-8) S.empty M.empty

initialMs :: LiveBlock -> CGMachineState
initialMs (LB _phi instrs _nb _pb) = CGMS M.empty M.empty instrs [] 0


getBlock :: Label -> CGMonad LiveBlock
getBlock lbl = do
    st <- get
    return $ CGDef.blocks st ! lbl

getLiveIn :: Label -> CGMonad (Set IRAddr)
getLiveIn lbl = do
    st <- get
    return $ CGDef.liveIn st ! lbl


freshStringLbl :: String -> CGMonad Label
freshStringLbl str = do
    st@CGS {nextStrLbl = nsl, strToLbl = s2l} <- get
    let newLbl = ".str_" ++ show nsl
    put $ st {strToLbl = M.insert str newLbl s2l, nextStrLbl = nsl+1}
    return newLbl


freshLbl :: CGMonad Label
freshLbl = do
    st@CGS {nextLbl = nl} <- get
    let newLbl = ".t_lbl_" ++ show nl
    put $ st {nextLbl = nl+1}
    return newLbl

getStringLbl :: String -> CGMonad Label
getStringLbl str = do
    CGS {strToLbl = s2l} <- get
    case M.lookup str s2l of
        Nothing -> freshStringLbl str
        Just lbl -> return lbl

getStringMapping :: CGMonad (Map String Label)
getStringMapping = do
    CGS {strToLbl = s2l} <- get
    return s2l

getCurrentMs :: CGMonad CGMachineState
getCurrentMs = do
    CGS {currentMs = cms} <- get
    return cms

changeCurrentMs :: Label -> CGMonad ()
changeCurrentMs lbl = do
    nextCurrentMs <- getMs lbl
    st <- get
    put $ st {currentMs = nextCurrentMs}

setCurrentMs :: CGMachineState -> CGMonad ()
setCurrentMs cms = do
    st <- get
    put $ st {currentMs = cms}

saveCurrentMs :: Label -> CGMonad ()
saveCurrentMs lbl = do
    cms <- getCurrentMs
    setMs lbl cms

saveCurrentMsAsInitMs :: Label -> CGMonad ()
saveCurrentMsAsInitMs lbl = do
    cms <- getCurrentMs
    setInitMs lbl cms

getMs :: Label -> CGMonad CGMachineState
getMs lbl = do
    CGS {blockToMs = b2ms} <- get
    return $ b2ms ! lbl

setMs :: Label -> CGMachineState -> CGMonad ()
setMs lbl ms = do
    st@CGS {blockToMs = b2ms} <- get
    put $ st {blockToMs = M.insert lbl ms b2ms}

getInitMs :: Label -> CGMonad CGMachineState
getInitMs lbl = do
    CGS {blockToInitialMs = b2ims}<- get
    return $ b2ims ! lbl

setInitMs :: Label -> CGMachineState -> CGMonad ()
setInitMs lbl ms = do
    st@CGS {blockToInitialMs = b2ims} <- get
    put $ st {blockToInitialMs = M.insert lbl ms b2ims}

freshStackLoc :: CGMonad CGMem
freshStackLoc = do
    st@CGS {nextStackLoc = nloc} <- get
    put $ st {nextStackLoc = nloc-8}
    return $ Mem RBP nloc

resetStackLoc :: CGMonad ()
resetStackLoc = do
    st <- get
    put $ st {nextStackLoc = -8, nonvolatileRegsUsed = S.empty}    

getLocSize :: CGMonad Integer
getLocSize = do
    CGS {nextStackLoc = nloc} <- get
    return $ if nloc `mod` 16 == 8 then nloc + 8 else nloc

addNonvolatileReg :: CGReg -> CGMonad ()
addNonvolatileReg reg = do
    st@CGS {nonvolatileRegsUsed = nru} <- get
    put $ st {nonvolatileRegsUsed = S.insert reg nru}

getUsedNonvolatileRegs :: CGMonad [CGReg]
getUsedNonvolatileRegs = do
    CGS {nonvolatileRegsUsed = nru} <- get
    return $ S.toList nru

saveCurrentNru :: Label -> CGMonad ()
saveCurrentNru lbl = do
    st@CGS {blToNru = btn} <- get
    nru <- getUsedNonvolatileRegs
    put $ st {blToNru = M.insert lbl nru btn}

restoreNru :: Label -> CGMonad ()
restoreNru lbl = do
    st@CGS {blToNru = nruMap} <- get
    let newNru = S.fromList $ nruMap ! lbl
    put $ st {nonvolatileRegsUsed = newNru}

freeRegistersPool :: CGMonad (Set CGReg)
freeRegistersPool = do
    CGMS {regToVar = r2v} <- getCurrentMs
    return $ S.difference registerPool (S.fromList (M.keys r2v))


addInstr :: AsmInstr -> CGMonad ()
addInstr instr = do
    cms@CGMS {generatedCode = gc} <- getCurrentMs
    setCurrentMs $ cms {generatedCode = instr:gc}

addInstrs :: [AsmInstr] -> CGMonad ()
addInstrs = mapM_ addInstr

getNextInstr :: CGMonad (Maybe (IRInstr, LiveMap))
getNextInstr = do
    CGMS {nextInstrs = ni} <- getCurrentMs
    case ni of
        (i:_) -> return $ Just i
        _ -> return Nothing

setNextInstr :: (IRInstr, LiveMap) -> CGMonad ()
setNextInstr i = do
    cms@CGMS {nextInstrs = is} <- getCurrentMs
    setCurrentMs $ cms {nextInstrs = i:is}

removeNextInstr :: CGMonad ()
removeNextInstr = do
    cms@CGMS {nextInstrs = _:is}<- getCurrentMs
    setCurrentMs $ cms {nextInstrs = is}


genMovOrLea :: CGMem -> CGMem -> [AsmInstr]
genMovOrLea dst src = case src of
    Obj _ -> [ Lea dst src ]
    Mem {} -> case dst of
        Mem {} -> [ Mov (Reg R10) src
                 , Mov dst (Reg R10) ]
        _     -> [ Mov dst src ]
    _ -> [ Mov dst src ]


intLiteral :: Integer -> CGMem
intLiteral int = Lit $ CGInt int

boolLiteral :: Bool -> CGMem
boolLiteral b = Lit $ CGBool b


getFreshRegister :: IRAddr -> CGMonad CGMem
getFreshRegister var = do
    freeRegisters <- freeRegistersPool
    let reg = S.elemAt 0 freeRegisters
    let mem = Reg reg
    addVarMapping var mem
    when (reg `elem` nonvolatileRegisters) $ addNonvolatileReg reg
    return mem

addVarMapping :: IRAddr -> CGMem -> CGMonad ()
addVarMapping var mem = case var of
    Indirect _ -> do
        cms@CGMS {regToVar = r2v, varToMem = v2m} <- getCurrentMs
        case mem of
            Reg reg -> do
                let newR2v = M.insert reg var r2v
                let newV2m = M.insert var mem v2m
                setCurrentMs $ cms {regToVar = newR2v, varToMem = newV2m}
            _ -> do
                let newV2m = M.insert var mem v2m
                setCurrentMs $ cms {varToMem = newV2m}
    _ -> return ()

remVarMapping :: IRAddr -> CGMonad ()
remVarMapping var = case var of
    Indirect _ -> do
        cms@CGMS {regToVar = r2v, varToMem = v2m} <- getCurrentMs
        let newR2v = M.filter (/= var) r2v
        let newV2m = M.filterWithKey (\k _ -> k /= var) v2m
        setCurrentMs $ cms {regToVar = newR2v, varToMem = newV2m}
    _ -> return ()

expireOld :: LiveMap -> CGMonad ()
expireOld lm = do
    cms@CGMS {regToVar = r2v, varToMem = v2m} <- getCurrentMs
    let newR2v = M.filter (`M.member` lm) r2v
    let newV2m = M.filterWithKey (\k _ -> k `M.member` lm) v2m
    setCurrentMs $ cms {regToVar = newR2v, varToMem = newV2m}

spill :: IRAddr -> LiveMap -> CGMonad CGMem
spill newVar lm = if M.null lm 
    then getFreshRegister newVar
    else do
        let nvInt = M.findWithDefault 0 newVar lm
        CGMS {regToVar = r2v, varToMem = v2m} <- getCurrentMs
        let (_, v0) = M.elemAt 0 r2v
        let (var, maxInt) = M.foldrWithKey (\_ v (vm, mi) -> do
                let varInt = lm ! v
                if varInt > mi 
                    then (v, varInt) 
                    else (vm, mi)) (v0, 0) (M.filter (`M.member` lm) r2v)
        if nvInt < maxInt 
            then do
                newLoc <- freshStackLoc
                moveVar var newLoc (v2m ! var)
                getFreshRegister newVar
            else do
                newLoc <- freshStackLoc
                addVarMapping newVar newLoc
                return newLoc 

moveVar :: IRAddr -> CGMem -> CGMem -> CGMonad ()
moveVar var dst src = do
    addInstrs $ genMovOrLea dst src
    remVarMapping var
    addVarMapping var dst

remapVar :: IRAddr -> CGMem -> CGMonad ()
remapVar var dst = do
    remVarMapping var
    addVarMapping var dst

getMemoryLoc :: IRAddr -> LiveMap -> CGMonad CGMem
getMemoryLoc var lm = case var of
    ImmInt int -> return $ intLiteral int
    ImmString str -> do
        lbl <- getStringLbl str
        return $ Obj lbl
    ImmBool b -> return $ boolLiteral b
    NoRet -> return $ intLiteral 0
    Indirect _ -> do
        CGMS {varToMem = v2m} <- getCurrentMs
        case M.lookup var v2m of
            Nothing -> do
                fr <- freeRegistersPool
                if S.null fr
                    then spill var lm
                    else getFreshRegister var
            Just mem -> return mem
