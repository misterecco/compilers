module CodeGen where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )

data CGReg = RDI | RSI | RDX | RCX | R8 | R9 | RAX | R10 | R11
            | RBX | R12 | R13 | R14 | R15 | RSP | RBP
    deriving (Show, Eq, Ord)

data CGMem = Reg CGReg | Mem CGReg Integer | Lit String
    deriving (Eq, Ord)

instance Show CGMem where
    show m = case m of
        Reg reg -> "%" ++ show reg
        Mem reg int -> show int ++ "(%" ++ show reg ++ ")"
        Lit lit -> lit

data AsmInstr 
    = Mov CGMem CGMem 
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
    | Push CGMem
    | Pop CGMem
    | Section String
    | Str String
    | Leave
    | Ret
    | Global Label
    | Lbl Label
    deriving (Eq, Ord)

instance Show AsmInstr where
    show i = case i of
        Mov dst src    -> "  movq " ++ show src ++ ", " ++ show dst
        Add dst src    -> "  addq " ++ show src ++ ", " ++ show dst
        Sub dst src    -> "  subq " ++ show src ++ ", " ++ show dst
        Imul dst src   -> "  imulq " ++ show src ++ ", " ++ show dst
        Idiv reg       -> "  idivq " ++ show reg
        Cdq            -> "  cdqq"
        Call lbl       -> "  call " ++ lbl
        Xchg larg rarg -> "  xchgq " ++ show larg ++ ", " ++ show rarg
        Cmp larg rarg  -> "  cmpq " ++ show larg ++ ", " ++ show rarg
        Jmp lbl        -> "  jmp " ++ lbl
        Jg lbl         -> "  jg " ++ lbl
        Jl lbl         -> "  jl " ++ lbl
        Jle lbl        -> "  jle " ++ lbl
        Jge lbl        -> "  jge " ++ lbl
        Push mem       -> "  pushq " ++ show mem
        Pop mem        -> "  popq " ++ show mem
        Section str    -> ".section " ++ str
        Str str        -> "  .string " ++ show str
        Leave          -> "  leave"
        Ret            -> "  ret"
        Global lbl     -> ".globl " ++ lbl
        Lbl lbl        -> lbl ++ ":"


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
    currentMs :: CGMachineState,
    blockToMs :: Map Label CGMachineState,
    blockToInitialMs :: Map Label CGMachineState,
    nextStackLoc :: Integer
}

type CGMonad = State CGState

registerPool :: Set CGReg
registerPool = S.fromList [RDI, RSI, RCX, R8, R9, RAX, R10, R11,
            RBX, R12, R13, R14, R15]

nonvolatileRegisters :: [CGReg]
nonvolatileRegisters = [RBX, R12, R13, R14, R15]

volatileRegisters :: [CGReg]
volatileRegisters = [RDI, RSI, RCX, R8, R9, R10, R11]

initialMs :: LiveBlock -> CGMachineState
initialMs (LB _phi instrs _nb _pb) = CGMS M.empty M.empty instrs []

initialCGState :: LiveState -> [Label] -> CGState
initialCGState (LS _ lm bl) ord = do
    let initMss = M.fromList $ Prelude.map (\lbl -> (lbl, initialMs (bl ! lbl))) ord
    let initMainMs = initMss ! "main"
    CGS bl lm M.empty 0 initMainMs initMss M.empty (-48)

getBlock :: Label -> CGMonad LiveBlock
getBlock lbl = do
    CGS bls _ _ _ _ _ _ _ <- get
    return $ bls ! lbl

getLiveIn :: Label -> CGMonad (Set IRAddr)
getLiveIn lbl = do
    CGS _ lm _ _ _ _ _ _ <- get
    return $ lm ! lbl

freshStringLbl :: String -> CGMonad Label
freshStringLbl str = do
    CGS bl lm s2l nsl cms b2ms b2ims nloc <- get
    let newLbl = "str_" ++ show nsl
    put $ CGS bl lm (M.insert str newLbl s2l) (nsl+1) cms b2ms b2ims nloc
    return newLbl

getStringLbl :: String -> CGMonad Label
getStringLbl str = do
    CGS _ _ s2l _ _ _ _ _ <- get
    case M.lookup str s2l of
        Nothing -> freshStringLbl str
        Just lbl -> return lbl

getCurrentMs :: CGMonad CGMachineState
getCurrentMs = do
    CGS _ _ _ _ cms _ _ _ <- get
    return cms

changeCurrentMs :: Label -> CGMonad ()
changeCurrentMs lbl = do
    nextCurrentMs <- getMs lbl
    CGS lb lm s2l nsl _ b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl nextCurrentMs b2ms b2ims nloc

setCurrentMs :: CGMachineState -> CGMonad ()
setCurrentMs cms = do
    CGS lb lm s2l nsl _ b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl cms b2ms b2ims nloc

saveCurrentMs :: Label -> CGMonad ()
saveCurrentMs lbl = do
    cms <- getCurrentMs
    setMs lbl cms

getMs :: Label -> CGMonad CGMachineState
getMs lbl = do
    CGS _ _ _ _ _ b2ms _ _ <- get
    return $ b2ms ! lbl

setMs :: Label -> CGMachineState -> CGMonad ()
setMs lbl ms = do
    CGS lb lm s2l nsl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl cms (M.insert lbl ms b2ms) b2ims nloc

getInitMs :: Label -> CGMonad CGMachineState
getInitMs lbl = do
    CGS _ _ _ _ _ _ b2ims _<- get
    return $ b2ims ! lbl

setInitMs :: Label -> CGMachineState -> CGMonad ()
setInitMs lbl ms = do
    CGS lb lm s2l nsl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl cms b2ms (M.insert lbl ms b2ims) nloc

freshStackLoc :: CGMonad CGMem
freshStackLoc = do
    CGS lb lm s2l nsl cms b2ms b2ims nloc <- get
    put $ CGS lb lm s2l nsl cms b2ms b2ims (nloc-8)
    return $ Mem RBP nloc

resetStackLoc :: CGMonad ()
resetStackLoc = do
    CGS lb lm s2l nsl cms b2ms b2ims _ <- get
    put $ CGS lb lm s2l nsl cms b2ms b2ims (-48)    

getLocSize :: CGMonad Integer
getLocSize = do
    CGS _ _ _ _ _ _ _ nloc <- get
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

removeNextInstr :: CGMonad ()
removeNextInstr = do
    CGMS r2v v2m (i:is) gc <- getCurrentMs
    setCurrentMs $ CGMS r2v v2m is gc

expireOld :: LiveMap -> CGMonad ()
expireOld lm = do
    CGMS r2v v2m is gc <- getCurrentMs
    let newR2v = M.filter (`M.member` lm) r2v
    let newV2m = M.filterWithKey (\k _ -> k `M.member` lm) v2m
    setCurrentMs $ CGMS newR2v newV2m is gc

addVarMapping :: IRAddr -> CGMem -> CGMonad ()
addVarMapping var mem = case var of
    Indirect _ -> do
        CGMS r2v v2m is gc <- getCurrentMs
        case mem of
            Reg reg -> do
                let newR2v = M.insert reg var r2v
                let newV2m = M.insert var mem v2m
                setCurrentMs $ CGMS newR2v newV2m is gc
            _ -> do
                let newV2m = M.insert var mem v2m
                setCurrentMs $ CGMS r2v newV2m is gc
    _ -> return ()

remVarMapping :: IRAddr -> CGMonad ()
remVarMapping var = case var of
    Indirect _ -> do
        CGMS r2v v2m is gc <- getCurrentMs
        let newR2v = M.filter (/= var) r2v
        let newV2m = M.filterWithKey (\k _ -> k /= var) v2m
        setCurrentMs $ CGMS newR2v newV2m is gc
    _ -> return ()

getFreshRegister :: IRAddr -> CGMonad CGMem
getFreshRegister var = do
    CGMS r2v _ _ _ <- getCurrentMs
    let freeRegisters = S.difference registerPool (S.fromList (M.keys r2v))
    let reg = S.elemAt 0 freeRegisters
    let mem = Reg reg
    addVarMapping var mem
    return mem

spill :: IRAddr -> LiveMap -> CGMonad CGMem
spill newVar lm = if M.null lm 
    then getFreshRegister newVar
    else do
        let nvInt = M.findWithDefault 0 newVar lm
        CGMS r2v _ _ _ <- getCurrentMs
        let (_, v0) = M.elemAt 0 r2v
        let (var, maxInt) = M.foldrWithKey (\_ v (var, maxInt) -> do
            let varInt = lm ! v
            if varInt > maxInt 
            then (v, varInt) 
            else (var, maxInt)) (v0, lm ! v0) r2v
        if nvInt < maxInt 
        then do
            newLoc <- freshStackLoc
            moveVar var newLoc
            getFreshRegister newVar
        else do
            newLoc <- freshStackLoc
            addVarMapping newVar newLoc
            return newLoc 

moveVar :: IRAddr -> CGMem -> CGMonad ()
moveVar var dst = do
    remVarMapping var
    addVarMapping var dst


getMemoryLoc :: IRAddr -> LiveMap -> CGMonad CGMem
getMemoryLoc var lm = case var of
    ImmInt int -> return $ Lit $ "$" ++ show int
    ImmString str -> do
        lbl <- getStringLbl str
        return $ Lit lbl
    ImmBool b -> return $ Lit $ "$" ++ if b then "1" else "0"
    NoRet -> return $ Lit "$0"
    Indirect _ -> do
        CGMS _ v2m _ _ <- getCurrentMs
        case M.lookup var v2m of
            Nothing -> if S.size registerPool == M.size lm
                then spill var lm
                else getFreshRegister var
            Just mem -> return mem

intLiteral :: Integer -> CGMem
intLiteral int = Lit $ "$" ++ show int

genInstrs :: (IRInstr, LiveMap) -> CGMonad ()
genInstrs (i, lm) = do
    expireOld lm
    case i of
        -- IRAss op dst larg rarg -> if dst `M.notMember` lm 
        --     then return ()
        --     else case dst of
        IRSAss op dst arg -> if dst `M.notMember` lm
            then return ()
            else do
                dstMem <- getMemoryLoc dst lm
                argMem <- getMemoryLoc arg lm
                case op of
                    IRNot -> addInstrs [ Mov dstMem (intLiteral 1)
                                       , Sub dstMem argMem ]
                    IRNeg -> addInstrs [ Mov dstMem argMem
                                       , Imul dstMem (intLiteral (-1)) ]
        IRGoto lbl -> addInstr $ Jmp lbl
        IRCpy dst src -> if dst `M.notMember` lm
            then return ()
            else do
                dstMem <- getMemoryLoc dst lm
                argMem <- getMemoryLoc src lm
                addInstr $ Mov dstMem argMem
        IRLabel lbl -> addInstr $ Lbl lbl
        IRRet addr -> do
            let popReg = popRegisters nonvolatileRegisters
            case addr of
                NoRet -> addInstrs $ popReg ++ [ Leave
                                                , Ret]
                _     -> do
                            mem <- getMemoryLoc addr lm
                            addInstrs $ popReg ++ [ Mov (Reg RAX) mem 
                                                    , Leave
                                                    , Ret ]
        IRParam var int -> if var `M.notMember` lm
            then return ()
            else do
                let mem = case int of
                            1 -> Reg RDI
                            2 -> Reg RSI
                            3 -> Reg RDX
                            4 -> Reg RCX
                            5 -> Reg R8
                            6 -> Reg R9
                            _ -> do
                                let offset = (int - 6) * 8 + 16
                                Mem RBP offset
                addVarMapping var mem
        _ -> return ()

pushRegisters :: [CGReg] -> [AsmInstr]
pushRegisters = Prelude.map (\reg -> Push (Reg reg))

popRegisters :: [CGReg] -> [AsmInstr]
popRegisters = (Prelude.map (\reg -> Pop (Reg reg))) . reverse

addFuncPrologue :: Label -> CGMonad ()
addFuncPrologue lbl = do
    CGMS r2v v2m is gc <- getMs lbl
    ls <- getLocSize
    let sb = ls + 40
    let (funcLbl:restInstr) = reverse gc
    let pushRegs = pushRegisters nonvolatileRegisters
    let newGc = [ funcLbl 
                , Push (Reg RBP)
                , Mov (Reg RBP) (Reg RSP) ] ++ pushRegs ++
                (Sub (Reg RSP) (intLiteral sb)):restInstr
    setMs lbl $ CGMS r2v v2m is (reverse newGc)

firstPassOnBlock :: Label -> CGMonad ()
firstPassOnBlock lbl = do
    instr <- getNextInstr
    case instr of 
        Nothing -> return ()
        Just i -> do
            removeNextInstr
            case i of
                (IRIf {}, _)-> return ()
                (IRGoto {}, _) -> return ()
                _ -> do
                    genInstrs i
                    firstPassOnBlock lbl


firstPassOnFunc :: Set Label -> Set Label -> CGMonad (Set Label)
firstPassOnFunc next visited
    | S.null next = return visited
    | otherwise   = do
        let lbl = S.elemAt 0 next
        changeCurrentMs lbl
        firstPassOnBlock lbl
        saveCurrentMs lbl
        LB _ _ nb _ <- getBlock lbl
        let nbSet = S.fromList nb
        let n1 = S.delete lbl next
        let v1 = S.insert lbl visited
        let n2 = S.union n1 (S.difference nbSet v1)
        firstPassOnFunc n2 v1

firstPass :: [Label] -> Set Label -> CGMonad ()
firstPass [] _ = return ()
firstPass (b:bs) visited 
    | b `S.member` visited = firstPass bs visited
    | otherwise            = do
        resetStackLoc    
        v1 <- firstPassOnFunc (S.singleton b) visited
        addFuncPrologue b
        firstPass bs v1


collectCode :: Label -> CGMonad [AsmInstr]
collectCode lbl = do
    CGMS _ _ _ gc <- getMs lbl
    return $ reverse gc

collectAllCode :: [Label] -> CGMonad [AsmInstr]
collectAllCode = foldM (\acc lbl -> do
    code <- collectCode lbl
    return $ acc ++ code) []


genCode :: [Label] -> CGMonad [AsmInstr]
genCode ord = do
    firstPass ord S.empty
    -- TODO: second pass
    collectAllCode ord

generateAsm :: LiveState -> [Label] -> [AsmInstr]
generateAsm ls ord = evalState (genCode ord) (initialCGState ls ord)
