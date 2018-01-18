module CGState where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )

data CGReg = RBX | R12 | RDI | RSI | R13 | R14 | R15 |
             RDX | RCX | R8 | R9 | RAX | R10 | R11 | RSP | RBP
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
    | Cdq
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
        Cdq            -> "    cdqq"
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
    generatedCode :: [AsmInstr]
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
    CGS bl lm M.empty 0 0 initMainMs initMss M.empty (-48)

getBlock :: Label -> CGMonad LiveBlock
getBlock lbl = do
    CGS bls _ _ _ _ _ _ _ _ <- get
    return $ bls ! lbl

getLiveIn :: Label -> CGMonad (Set IRAddr)
getLiveIn lbl = do
    CGS _ lm _ _ _ _ _ _ _ <- get
    return $ lm ! lbl


initialMs :: LiveBlock -> CGMachineState
initialMs (LB _phi instrs _nb _pb) = CGMS M.empty M.empty instrs []

freshStringLbl :: String -> CGMonad Label
freshStringLbl str = do
    CGS bl lm s2l nsl nl cms b2ms b2ims nloc <- get
    let newLbl = ".str_" ++ show nsl
    put $ CGS bl lm (M.insert str newLbl s2l) (nsl+1) nl cms b2ms b2ims nloc
    return newLbl

freshLbl :: CGMonad Label
freshLbl = do
    CGS bl lm s2l nsl nl cms b2ms b2ims nloc <- get
    let newLbl = ".t_lbl_" ++ show nl
    put $ CGS bl lm s2l nsl (nl+1) cms b2ms b2ims nloc
    return newLbl

getStringLbl :: String -> CGMonad Label
getStringLbl str = do
    CGS _ _ s2l _ _ _ _ _ _ <- get
    case M.lookup str s2l of
        Nothing -> freshStringLbl str
        Just lbl -> return lbl

getStringMapping :: CGMonad (Map String Label)
getStringMapping = do
    CGS _ _ s2l _ _ _ _ _ _ <- get
    return s2l

getCurrentMs :: CGMonad CGMachineState
getCurrentMs = do
    CGS _ _ _ _ _ cms _ _ _ <- get
    return cms

changeCurrentMs :: Label -> CGMonad ()
changeCurrentMs lbl = do
    nextCurrentMs <- getMs lbl
    CGS lb lm s2l nsl nl _ b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nl nextCurrentMs b2ms b2ims nloc

setCurrentMs :: CGMachineState -> CGMonad ()
setCurrentMs cms = do
    CGS lb lm s2l nsl nl _ b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nl cms b2ms b2ims nloc

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
    CGS _ _ _ _ _ _ b2ms _ _ <- get
    return $ b2ms ! lbl

setMs :: Label -> CGMachineState -> CGMonad ()
setMs lbl ms = do
    CGS lb lm s2l nsl nl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nl cms (M.insert lbl ms b2ms) b2ims nloc

getInitMs :: Label -> CGMonad CGMachineState
getInitMs lbl = do
    CGS _ _ _ _ _ _ _ b2ims _<- get
    return $ b2ims ! lbl

setInitMs :: Label -> CGMachineState -> CGMonad ()
setInitMs lbl ms = do
    CGS lb lm s2l nsl nl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nl cms b2ms (M.insert lbl ms b2ims) nloc

freshStackLoc :: CGMonad CGMem
freshStackLoc = do
    CGS lb lm s2l nsl nl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nl cms b2ms b2ims (nloc-8)
    return $ Mem RBP nloc

resetStackLoc :: CGMonad ()
resetStackLoc = do
    CGS lb lm s2l nsl nl cms b2ms b2ims _ <- get
    put $ CGS lb lm s2l nsl nl cms b2ms b2ims (-48)    

getLocSize :: CGMonad Integer
getLocSize = do
    CGS _ _ _ _ _ _ _ _ nloc <- get
    return $ if nloc `mod` 16 == 8 then nloc + 8 else nloc

addInstr :: AsmInstr -> CGMonad ()
addInstr instr = do
    CGMS r2v v2m is gc <- getCurrentMs
    setCurrentMs $ CGMS r2v v2m is (instr:gc)

addInstrs :: [AsmInstr] -> CGMonad ()
addInstrs = mapM_ addInstr

getNextInstr :: CGMonad (Maybe (IRInstr, LiveMap))
getNextInstr = do
    CGMS _ _ ni _ <- getCurrentMs
    case ni of
        (i:_) -> return $ Just i
        _ -> return Nothing

setNextInstr :: (IRInstr, LiveMap) -> CGMonad ()
setNextInstr i = do
    CGMS r2v v2m is gc <- getCurrentMs
    setCurrentMs $ CGMS r2v v2m (i:is) gc

removeNextInstr :: CGMonad ()
removeNextInstr = do
    CGMS r2v v2m (_:is) gc <- getCurrentMs
    setCurrentMs $ CGMS r2v v2m is gc


genMovOrLea :: CGMem -> CGMem -> AsmInstr
genMovOrLea dst src = case src of
    Obj _ -> Lea dst src
    _ -> Mov dst src
