module CodeGen where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )
import CGState
import CFG ( Phi )

registerPool :: Set CGReg
registerPool = S.fromList [RDI, RSI, RCX, R8, R9, RAX, R10, R11,
            RBX, R12, R13, R14, R15]

nonvolatileRegisters :: [CGReg]
nonvolatileRegisters = [RBX, R12, R13, R14, R15]

volatileRegisters :: [CGReg]
volatileRegisters = [RDI, RSI, RCX, R8, R9, R10, R11]

paramRegisters :: [CGReg]
paramRegisters = [RDI, RSI, RDX, RCX, R8, R9]


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


callStringFunction :: Label -> LiveMap -> IRAddr -> IRAddr -> CGMonad ()
callStringFunction lbl lm larg rarg = do
    preserveRegisters lm
    prepareRegisterArgs [larg, rarg] lm
    addInstr $ Call lbl
    restoreRegisters lm

genInstrs :: (IRInstr, LiveMap) -> CGMonad ()
genInstrs (i, lm) = case i of
    IRAss op dst larg rarg -> if dst `M.notMember` lm 
        then return ()
        else case addrType dst of
            IRStr -> do
                callStringFunction "__concat__" lm larg rarg
                dstMem <- getMemoryLoc dst lm
                addInstr $ Mov dstMem (Reg RAX)
            IRInt -> do
                dstMem <- getMemoryLoc dst lm
                largMem <- getMemoryLoc larg lm
                rargMem <- getMemoryLoc rarg lm
                case op of
                    IRAdd -> addInstrs [ Mov dstMem largMem
                                        , Add dstMem rargMem ]
                    IRSub -> addInstrs [ Mov dstMem largMem
                                        , Sub dstMem rargMem ]
                    IRMul -> addInstrs [ Mov dstMem largMem
                                        , Imul dstMem rargMem ]
                    IRDiv -> addInstrs [ Mov (Reg RAX) largMem
                                        , Cdq
                                        , Idiv rargMem
                                        , Mov dstMem (Reg RAX) ]
                    IRMod -> addInstrs [ Mov (Reg RAX) largMem
                                        , Cdq
                                        , Idiv rargMem
                                        , Mov dstMem (Reg RDX) ]
            _ -> return ()
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
    IRCall dst func args -> do
        let n = toInteger (length args)
        preserveRegisters lm
        pushRestArgs (drop 6 args) lm
        prepareRegisterArgs (take 6 args) lm
        addInstr $ Call func
        popRestArgs n
        restoreRegisters lm
        if dst == NoRet || dst `M.notMember` lm
            then return ()
            else do
                dstMem <- getMemoryLoc dst lm
                addInstr $ Mov dstMem (Reg RAX)
    IRIf cmp larg rarg lTrue lFalse -> do
        case addrType larg of
            IRStr -> do
                callStringFunction "__strcmp__" lm larg rarg
                addInstr $ Cmp (Reg RAX) (intLiteral 0)
            _ -> do
                largMem <- getMemoryLoc larg lm
                rargMem <- getMemoryLoc rarg lm
                addInstr $ Cmp largMem rargMem
        case cmp of
            IRGt -> addInstr $ Jg lTrue
            IRLt -> addInstr $ Jl lTrue
            IRGe -> addInstr $ Jge lTrue
            IRLe -> addInstr $ Jle lTrue
            IREq -> addInstr $ Jeq lTrue
            IRNe -> addInstr $ Jne lTrue
        addInstr $ Jmp lFalse  
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
                        addInstrs $ (Mov (Reg RAX) mem):popReg ++ 
                                                [ Leave
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


pushRegisters :: [CGReg] -> [AsmInstr]
pushRegisters = Prelude.map (\reg -> Push (Reg reg))

popRegisters :: [CGReg] -> [AsmInstr]
popRegisters = (Prelude.map (\reg -> Pop (Reg reg))) . reverse

prepareRegisterArgs :: [IRAddr] -> LiveMap -> CGMonad ()
prepareRegisterArgs args lm = mapM_ (\(arg, reg) -> do
    argMem <- getMemoryLoc arg lm
    addInstr $ Mov (Reg reg) argMem) (zip args paramRegisters)

pushRestArgs :: [IRAddr] -> LiveMap -> CGMonad ()
pushRestArgs args lm = do
    let n = toInteger $ length args
    if n == 0 
        then return ()
        else do
            mapM_ (\arg -> do
                argMem <- getMemoryLoc arg lm
                addInstr $ Push argMem) (reverse args)
            if n `mod` 2 == 0 
                then return ()
                else addInstr $ Sub (Reg RSP) (intLiteral 8)

popRestArgs :: Integer -> CGMonad ()
popRestArgs n = if n <= 6 
    then return ()
    else do
        let alignedN = if n `mod` 2 == 0 then n-6 else n-5
        let sub = 8 * alignedN
        addInstr $ Add (Reg RSP) (intLiteral sub)

getActiveVolatileRegs :: LiveMap -> CGMonad [CGReg]
getActiveVolatileRegs lm = do
    CGMS r2v _ _ _ <- getCurrentMs
    return $ Prelude.filter (\reg -> 
            reg `M.member` r2v && (r2v ! reg) `M.member` lm) volatileRegisters

preserveRegisters :: LiveMap -> CGMonad ()
preserveRegisters lm = do
    regs <- getActiveVolatileRegs lm
    addInstrs $ pushRegisters regs

restoreRegisters :: LiveMap -> CGMonad ()
restoreRegisters lm = do
    regs <- getActiveVolatileRegs lm
    addInstrs $ popRegisters regs


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
    allocateLiveIns lbl    
    doSteps
  where 
    doSteps :: CGMonad ()
    doSteps = do
        instr <- getNextInstr
        case instr of 
            Nothing -> return ()
            Just i@(_, lm) -> case i of
                (IRIf {}, _)-> return ()
                (IRGoto {}, _) -> return ()
                _ -> do
                    removeNextInstr
                    genInstrs i
                    expireOld lm                
                    doSteps

secondPassOnBlock :: Label -> CGMonad ()
secondPassOnBlock lbl = do
    instr <- getNextInstr
    case instr of 
        Nothing -> return ()
        Just i@(_, lm) -> do
            removeNextInstr
            genInstrs i
            expireOld lm            
            secondPassOnBlock lbl

passOnFunc :: (Label -> CGMonad ()) -> Set Label -> Set Label -> CGMonad (Set Label)
passOnFunc passOnBlock next visited
    | S.null next = return visited
    | otherwise   = do
        let lbl = S.elemAt 0 next
        changeCurrentMs lbl
        passOnBlock lbl
        saveCurrentMs lbl
        LB _ _ nb _ <- getBlock lbl
        let nbSet = S.fromList nb
        let n1 = S.delete lbl next
        let v1 = S.insert lbl visited
        let n2 = S.union n1 (S.difference nbSet v1)
        passOnFunc passOnBlock n2 v1

allocateLiveIns :: Label -> CGMonad ()
allocateLiveIns lbl = do
    changeCurrentMs lbl
    liveIns <- getLiveIn lbl
    let zeros = 0:zeros
    let lm = M.fromList (zip (S.toList liveIns) zeros)
    mapM_ (\k -> getMemoryLoc k lm) (S.toList liveIns)
    saveCurrentMsAsInitMs lbl
    saveCurrentMs lbl

firstPass :: [Label] -> Set Label -> CGMonad ()
firstPass [] _ = return ()
firstPass (b:bs) visited 
    | b `S.member` visited = firstPass bs visited
    | otherwise = do
        resetStackLoc    
        v1 <- passOnFunc firstPassOnBlock (S.singleton b) visited
        addFuncPrologue b
        firstPass bs v1

secondPass :: [Label] -> Set Label -> CGMonad ()
secondPass [] _ = return ()
secondPass (b:bs) visited 
    | b `S.member` visited = secondPass bs visited
    | otherwise = do
        v1 <- passOnFunc secondPassOnBlock (S.singleton b) visited
        secondPass bs v1


calculateMemMapping :: Label -> Label -> CGMonad (Map CGMem CGMem)
calculateMemMapping src dst = do
    CGMS _ v2mDst _ _ <- getInitMs dst
    CGMS _ v2mSrc _ _ <- getCurrentMs    
    LB phi _ _ _ <- getBlock dst            
    calcMap v2mSrc phi (M.toList v2mDst)
  where
    calcMap :: Map IRAddr CGMem -> Phi -> [(IRAddr, CGMem)] -> CGMonad (Map CGMem CGMem)
    calcMap v2mSrc phi = foldM (\acc (var, mem) ->
        if var `M.member` v2mSrc
            then do
                let srcMem = v2mSrc ! var
                return $ M.insert srcMem mem acc
            else do
                let pm = findPhiMapping phi var
                case pm of
                    Nothing -> return acc
                    Just srcVar -> do
                        srcMem <- case srcVar of
                            ImmInt int -> return $ intLiteral int
                            ImmString str -> do
                                lbl <- getStringLbl str
                                return $ Lit lbl
                            ImmBool b -> return $ Lit $ "$" ++ if b then "1" else "0"
                            NoRet -> return $ Lit "$0"
                            Indirect _ -> return $ v2mSrc ! srcVar
                        return $ M.insert srcMem mem acc
            ) M.empty
    findPhiMapping :: Phi -> IRAddr -> Maybe IRAddr
    findPhiMapping phi var = case M.lookup var phi of
        Nothing -> Nothing
        Just bindings -> do
            let bindMap = M.fromList bindings
            M.lookup src bindMap

addMemSwaps :: Label -> Label -> CGMonad ()
addMemSwaps src dst = do
    mapping <- calculateMemMapping src dst
    cycleOnlyMapping <- addMovs mapping
    addXchgs cycleOnlyMapping
    return ()

addXchgs :: Map CGMem CGMem -> CGMonad ()
addXchgs mapping = if M.null mapping
    then return ()
    else do
        let (cStart, _) = M.elemAt 0 mapping
        newMapping <- addXchg cStart cStart mapping
        addXchgs newMapping

addXchg :: CGMem -> CGMem -> Map CGMem CGMem -> CGMonad (Map CGMem CGMem)
addXchg cEnd next mapping = if mapping ! next == cEnd
    then return $ M.delete next mapping
    else do
        let dst = mapping ! next
        addInstr $ Xchg dst next
        addXchg cEnd dst (M.delete next mapping)

addMovs :: Map CGMem CGMem -> CGMonad (Map CGMem CGMem)
addMovs mapping = do
    m1 <- addMov mapping
    if m1 == mapping 
        then return mapping
        else addMovs m1

addMov :: Map CGMem CGMem -> CGMonad (Map CGMem CGMem)
addMov mapping = do
    let leaves = M.filter (\v -> M.notMember v mapping) mapping
    mapM_ (\(src, dst) -> addInstr $ Mov dst src) (M.toList leaves)
    return $ mapping M.\\ leaves

addPermutation :: Label -> CGMonad ()
addPermutation lbl = do
    changeCurrentMs lbl
    LB _ _ nb _ <- getBlock lbl
    mapM_ (addMemSwaps lbl) nb
    saveCurrentMs lbl    

addPermutations :: [Label] -> CGMonad ()
addPermutations = mapM_ addPermutation

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
    addPermutations ord
    secondPass ord S.empty
    collectAllCode ord

generateAsm :: LiveState -> [Label] -> [AsmInstr]
generateAsm ls ord = evalState (genCode ord) (initialCGState ls ord)
