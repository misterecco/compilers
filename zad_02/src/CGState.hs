module CGState where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )

data CGReg = RBX | R12 | R13 | R14 | R15 | RAX | R10 | R11 |
             RDI | RSI | RDX | RCX | R8 | R9 | RSP | RBP
    deriving (Show, Eq, Ord)

data CGMem = Reg CGReg | Mem CGReg Integer | Lit String | Obj Label
    deriving (Eq, Ord)

instance Show CGMem where
    show m = case m of
        Reg reg -> "%" ++ show reg
        Mem reg int -> show int ++ "(%" ++ show reg ++ ")"
        Lit lit -> lit
        Obj lbl -> lbl

data AsmInstr 
    = Mov CGMem CGMem
    | Lea CGMem CGMem
    | Add CGMem CGMem
    | Sub CGMem CGMem
    | Imul CGMem CGMem
    | Idiv CGMem
    | Cqo
    | Call Label
    | Xchg CGMem CGMem
    | Cmp CGMem CGMem
    | Jmp Label 
    | Jg Label
    | Jl Label
    | Jle Label 
    | Jge Label
    | Jeq Label
    | Jne Label
    | Push CGMem
    | Pop CGMem
    | Section String
    | Str String
    | Leave
    | Ret
    | Global Label
    | Lbl Label
    | P2Align
    | Function Label
    deriving (Eq, Ord)

instance Show AsmInstr where
    show i = case i of  
        Mov dst src    -> "    movq " ++ show src ++ ", " ++ show dst
        Lea dst src    -> "    leaq " ++ show src ++ ", " ++ show dst
        Add dst src    -> "    addq " ++ show src ++ ", " ++ show dst
        Sub dst src    -> "    subq " ++ show src ++ ", " ++ show dst
        Imul dst src   -> "    imulq " ++ show src ++ ", " ++ show dst
        Idiv reg       -> "    idivq " ++ show reg
        Cqo            -> "    cqo"
        Call lbl       -> "    call " ++ lbl
        Xchg larg rarg -> "    xchgq " ++ show rarg ++ ", " ++ show larg
        Cmp larg rarg  -> "    cmpq " ++ show rarg ++ ", " ++ show larg
        Jmp lbl        -> "    jmp " ++ lbl
        Jg lbl         -> "    jg " ++ lbl
        Jl lbl         -> "    jl " ++ lbl
        Jle lbl        -> "    jle " ++ lbl
        Jge lbl        -> "    jge " ++ lbl
        Jeq lbl        -> "    je " ++ lbl
        Jne lbl        -> "    jne " ++ lbl
        Push mem       -> "    pushq " ++ show mem
        Pop mem        -> "    popq " ++ show mem
        Section str    -> ".section " ++ str
        Str str        -> "    .string " ++ str
        Leave          -> "    leave"
        Ret            -> "    ret"
        Global lbl     -> "    .globl " ++ lbl
        Lbl lbl        -> lbl ++ ":"
        P2Align        -> "    .p2align 4,,15"
        Function name  -> "    .type " ++ name ++ ", @function"
        

data CGMachineState = CGMS {
    regToVar :: Map CGReg IRAddr,
    varToMem :: Map IRAddr CGMem,
    nextInstrs :: [(IRInstr, LiveMap)],
    generatedCode :: [AsmInstr],
    currentStackPos :: Integer
}

data CGState = CGS {
    blocks :: Map Label LiveBlock,
    liveIn :: Map Label (Set IRAddr),
    strToLbl :: Map String Label,
    nextStrLbl :: Integer,
    nextLbl :: Integer,
    currentMs :: CGMachineState,
    blockToMs :: Map Label CGMachineState,
    blockToInitialMs :: Map Label CGMachineState,
    nextStackLoc :: Integer
}

type CGMonad = State CGState


initialCGState :: LiveState -> [Label] -> CGState
initialCGState (LS _ lm bl) ord = do
    let initMss = M.fromList $ Prelude.map (\lbl -> (lbl, initialMs (bl ! lbl))) ord
    let initMainMs = initMss ! "main"
    CGS bl lm M.empty 0 0 initMainMs initMss M.empty (-8)

initialMs :: LiveBlock -> CGMachineState
initialMs (LB _phi instrs _nb _pb) = CGMS M.empty M.empty instrs [] 0


getBlock :: Label -> CGMonad LiveBlock
getBlock lbl = do
    st <- get
    return $ CGState.blocks st ! lbl

getLiveIn :: Label -> CGMonad (Set IRAddr)
getLiveIn lbl = do
    st <- get
    return $ CGState.liveIn st ! lbl


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
    put $ st {nextStackLoc = -8}    

getLocSize :: CGMonad Integer
getLocSize = do
    CGS {nextStackLoc = nloc} <- get
    return $ if nloc `mod` 16 == 8 then nloc + 8 else nloc

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


genMovOrLea :: CGMem -> CGMem -> AsmInstr
genMovOrLea dst src = case src of
    Obj _ -> Lea dst src
    _ -> Mov dst src
