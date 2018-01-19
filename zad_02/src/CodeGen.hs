module CodeGen where

import Control.Monad.State
import Data.Map as M
import Data.Set as S

import IRDef
import Live ( LiveMap, LiveState(..), LiveBlock(LB) )
import CGDef
import CGState
import CFG ( Phi )
import PeepHole


type MemMap = Map CGMem CGMem


genInstrs :: Label -> (IRInstr, LiveMap) -> CGMonad ()
genInstrs lbl (i, lm) = case i of
    IRAss op dst larg rarg -> if dst `M.notMember` lm 
        then return ()
        else case addrType dst of
            IRStr -> do
                callStringFunction "__concat__" lm larg rarg
                dstMem <- getMemoryLoc dst lm
                addInstr $ genMovOrLea dstMem (Reg RAX)
            IRInt -> do
                dstMem <- getMemoryLoc dst lm
                largMem <- getMemoryLoc larg lm
                rargMem <- getMemoryLoc rarg lm
                case op of
                    IRAdd -> addInstrs [ genMovOrLea dstMem largMem
                                        , Add dstMem rargMem ]
                    IRSub -> addInstrs [ genMovOrLea dstMem largMem
                                        , Sub dstMem rargMem ]
                    IRMul -> addInstrs [ genMovOrLea dstMem largMem
                                        , Imul dstMem rargMem ]
                    IRDiv -> do
                        addInstrs [ genMovOrLea (Reg RAX) largMem
                                  , Cqo ]
                        case rargMem of
                            Lit _ -> addInstrs [ genMovOrLea (Reg R10) rargMem
                                               , Idiv (Reg R10) ]
                            _ -> addInstr $ Idiv rargMem
                        addInstr $ genMovOrLea dstMem (Reg RAX)
                    IRMod -> do
                        addInstrs [ genMovOrLea (Reg RAX) largMem
                                  , Cqo ]
                        case rargMem of
                            Lit _ -> addInstrs [ genMovOrLea (Reg R10) rargMem
                                               , Idiv (Reg R10) ]
                            _ -> addInstr $ Idiv rargMem
                        addInstr $ genMovOrLea dstMem (Reg RDX)
            _ -> return ()
    IRSAss op dst arg -> if dst `M.notMember` lm
        then return ()
        else do
            dstMem <- getMemoryLoc dst lm
            argMem <- getMemoryLoc arg lm
            case op of
                IRNot -> addInstrs [ genMovOrLea dstMem (intLiteral 1)
                                    , Sub dstMem argMem ]
                IRNeg -> addInstrs [ genMovOrLea dstMem argMem
                                    , Imul dstMem (intLiteral (-1)) ]
    IRCall dst func args -> do
        let (regArgs, stackArgs) = splitAt 6 args
        preserveRegisters
        pushRestArgs lm stackArgs
        prepareRegisterArgs regArgs lm
        callFunc func
        popRestArgs stackArgs
        restoreRegisters
        if dst == NoRet || dst `M.notMember` lm
            then return ()
            else do
                dstMem <- getMemoryLoc dst lm
                addInstr $ genMovOrLea dstMem (Reg RAX)
    IRIf cmp larg rarg lTrue lFalse -> do
        tmpLbl <- freshLbl
        case addrType larg of
            IRStr -> do
                callStringFunction "__strcmp__" lm larg rarg
                addInstr $ Cmp (Reg RAX) (intLiteral 0)
            _ -> do
                largMem <- getMemoryLoc larg lm
                rargMem <- getMemoryLoc rarg lm                
                case largMem of
                    Lit _ -> do
                        addInstr $ genMovOrLea (Reg RAX) largMem
                        addInstr $ Cmp (Reg RAX) rargMem
                    _ -> addInstr $ Cmp largMem rargMem
        case cmp of
            IRGt -> addInstr $ Jg tmpLbl
            IRLt -> addInstr $ Jl tmpLbl
            IRGe -> addInstr $ Jge tmpLbl
            IRLe -> addInstr $ Jle tmpLbl
            IREq -> addInstr $ Jeq tmpLbl
            IRNe -> addInstr $ Jne tmpLbl
        addMemSwaps lbl lFalse
        addInstr $ Jmp lFalse 
        addInstr $ Lbl tmpLbl
        addMemSwaps lbl lTrue 
        addInstr $ Jmp lTrue
    IRGoto dst -> do
        addMemSwaps lbl dst
        addInstr $ Jmp dst
    IRCpy dst src -> if dst `M.notMember` lm
        then return ()
        else do
            dstMem <- getMemoryLoc dst lm
            argMem <- getMemoryLoc src lm
            addInstr $ genMovOrLea dstMem argMem
    IRLabel l -> addInstr $ Lbl l
    IRRet addr -> do
        popReg <- popNonvolatileRegs
        case addr of
            NoRet -> addInstrs $ popReg ++ [ Leave
                                            , Ret]
            _     -> do
                        mem <- getMemoryLoc addr lm
                        addInstrs $ (genMovOrLea (Reg RAX) mem):popReg ++ 
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


callFunc :: Label -> CGMonad ()
callFunc lbl = do
    CGMS {currentStackPos = csp} <- getCurrentMs
    if csp `mod` 16 == 0 
        then addInstr $ Call lbl
    else addInstrs [ Sub (Reg RSP) (intLiteral 8)
                   , Call lbl
                   , Add (Reg RSP) (intLiteral 8) ]


callStringFunction :: Label -> LiveMap -> IRAddr -> IRAddr -> CGMonad ()
callStringFunction lbl lm larg rarg = do
    preserveRegisters
    prepareRegisterArgs [larg, rarg] lm
    callFunc lbl
    restoreRegisters


pushNonvolatileRegs :: CGMonad [AsmInstr]
pushNonvolatileRegs = do
    nru <- getUsedNonvolatileRegs
    return $ Prelude.map (\reg -> Push (Reg reg)) nru

popNonvolatileRegs :: CGMonad [AsmInstr]
popNonvolatileRegs = do
    nru <- getUsedNonvolatileRegs
    return $ Prelude.map (\reg -> Pop (Reg reg)) . reverse $ nru


addMemSwaps :: Label -> Label -> CGMonad ()
addMemSwaps src dst = do
    mapping <- calculateMemMapping src dst
    cycleOnlyMapping <- addMovs mapping
    addXchgs cycleOnlyMapping

calculateMemMapping :: Label -> Label -> CGMonad MemMap
calculateMemMapping src dst = do
    CGMS {varToMem = v2mDst} <- getInitMs dst
    CGMS {varToMem = v2mSrc}<- getCurrentMs    
    LB phi _ _ _ <- getBlock dst
    calcMap v2mSrc phi (M.toList v2mDst)
  where
    calcMap :: Map IRAddr CGMem -> Phi -> [(IRAddr, CGMem)] -> CGMonad MemMap
    calcMap v2mSrc phi = foldM (\acc (var, mem) ->
        if var `M.member` v2mSrc
            then do
                let srcMem = v2mSrc ! var
                return $ M.insert mem srcMem acc
            else do
                let pm = findPhiMapping phi var
                case pm of
                    Nothing -> return acc
                    Just srcVar -> do
                        srcMem <- case srcVar of
                            ImmInt int -> return $ intLiteral int
                            ImmString str -> do
                                lbl <- getStringLbl str
                                return $ Obj lbl
                            ImmBool b -> return $ boolLiteral b
                            NoRet -> return $ intLiteral 0
                            Indirect _ -> return $ v2mSrc ! srcVar
                        return $ M.insert mem srcMem acc
            ) M.empty
    findPhiMapping :: Phi -> IRAddr -> Maybe IRAddr
    findPhiMapping phi var = case M.lookup var phi of
        Nothing -> Nothing
        Just bindings -> do
            let bindMap = M.fromList bindings
            M.lookup src bindMap

addXchgs :: Map CGMem CGMem -> CGMonad ()
addXchgs mapping = if M.null mapping
    then return ()
    else do
        let (cStart, _) = M.elemAt 0 mapping
        newMapping <- addXchg cStart cStart mapping
        addXchgs newMapping

addXchg :: CGMem -> CGMem -> Map CGMem CGMem -> CGMonad MemMap
addXchg cEnd next mapping = if mapping ! next == cEnd
    then return $ M.delete next mapping
    else do
        let dst = mapping ! next
        addInstr $ Xchg dst next
        addXchg cEnd dst (M.delete next mapping)

addMovs :: Map CGMem CGMem -> CGMonad MemMap
addMovs mapping = do
    m1 <- addMov mapping
    if m1 == mapping 
        then return mapping
        else addMovs m1

addMov :: Map CGMem CGMem -> CGMonad MemMap
addMov mapping = do
    let ks = S.fromList $ M.elems mapping
    let leaves = M.filterWithKey (\k _ -> S.notMember k ks) mapping
    mapM_ (\(dst, src) -> addInstr $ genMovOrLea dst src) (M.toList leaves)
    return $ mapping M.\\ leaves


pushStackLoc :: CGMonad CGMem
pushStackLoc = do
    cms@CGMS {varToMem = v2m, currentStackPos = csp} <- getCurrentMs
    let newCsp = csp - 8
    let newV2m = M.map (\mem -> case mem of
                            Mem RSP offset -> Mem RSP (offset+8)
                            _ -> mem) v2m
    setCurrentMs $ cms {varToMem = newV2m, currentStackPos = newCsp}
    currentStackLoc

popStackLoc :: CGMonad ()
popStackLoc = do
    cms@CGMS {varToMem = v2m, currentStackPos = csp} <- getCurrentMs
    let newV2m = M.map (\mem -> case mem of
                            Mem RSP offset -> Mem RSP (offset-8)
                            _ -> mem) v2m
    setCurrentMs $ cms {varToMem = newV2m, currentStackPos = csp + 8}

currentStackLoc :: CGMonad CGMem
currentStackLoc = return $ Mem RSP 0


pushVariable :: LiveMap -> IRAddr -> CGMonad ()
pushVariable lm arg = do
    argMem <- getMemoryLoc arg lm
    _ <- pushStackLoc
    addInstr $ Push argMem

popVariables :: [IRAddr] -> CGMonad ()
popVariables args = unless (Prelude.null args) $ do
    mapM_ (\_ -> popStackLoc) args
    let n = toInteger $ length args
    addInstr $ Add (Reg RSP) (intLiteral (n*8))


pushRegister :: CGReg -> CGMonad ()
pushRegister reg = do
    let regMem = Reg reg
    CGMS {regToVar = r2v} <- getCurrentMs
    tmpMem <- pushStackLoc
    when (reg `M.member` r2v) $ do
        let var = r2v ! reg
        remapVar var tmpMem regMem
    addInstr $ Push regMem

popRegister :: CGReg -> CGMonad ()
popRegister reg = do
    let regMem = Reg reg
    csl <- currentStackLoc
    CGMS {varToMem = v2m} <- getCurrentMs
    popStackLoc    
    let filteredV2m = M.filter (== csl) v2m
    unless (M.null filteredV2m) $ do
        let (var, _) = M.elemAt 0 filteredV2m
        remapVar var regMem csl
    addInstr $ Pop regMem

pushRegisters :: [CGReg] -> CGMonad ()
pushRegisters = mapM_ pushRegister

popRegisters :: [CGReg] -> CGMonad ()
popRegisters = (mapM_ popRegister) . reverse


prepareRegisterArgs :: [IRAddr] -> LiveMap -> CGMonad ()
prepareRegisterArgs args lm = mapM_ (\(arg, reg) -> do
    argMem <- getMemoryLoc arg lm
    addInstr $ genMovOrLea (Reg reg) argMem) (zip args paramRegisters)

pushRestArgs :: LiveMap -> [IRAddr] -> CGMonad ()
pushRestArgs lm = (mapM_ (pushVariable lm)) . reverse

popRestArgs :: [IRAddr] -> CGMonad ()
popRestArgs = popVariables

getActiveVolatileRegs :: CGMonad [CGReg]
getActiveVolatileRegs = do
    CGMS {regToVar = r2v} <- getCurrentMs
    return $ Prelude.filter (\reg -> reg `M.member` r2v ) volatileRegisters


preserveRegisters :: CGMonad ()
preserveRegisters = getActiveVolatileRegs >>= pushRegisters

restoreRegisters :: CGMonad ()
restoreRegisters = getActiveVolatileRegs >>= popRegisters


addFuncPrologue :: Label -> CGMonad ()
addFuncPrologue lbl = do
    cms@CGMS {generatedCode = gc} <- getMs lbl
    ls <- getLocSize
    pushRegs <- pushNonvolatileRegs
    let alignedLs = ls - ((toInteger $ length pushRegs) `mod` 2) * 8
    let (funcLbl:restInstr) = reverse gc
    let newGc = [ funcLbl 
                , Push (Reg RBP)
                , genMovOrLea (Reg RBP) (Reg RSP) ] ++ 
                (Add (Reg RSP) (intLiteral alignedLs)):pushRegs ++
                restInstr
    setMs lbl $ cms {generatedCode = reverse newGc}

firstPassOnBlock :: Label -> CGMonad ()
firstPassOnBlock lbl = do
    allocateLiveIns lbl    
    doSteps
  where 
    doSteps :: CGMonad ()
    doSteps = do
        instr <- getNextInstr
        case instr of 
            Nothing -> setNextInstr (IRRet NoRet, M.empty)
            Just i@(_, lm) -> case i of
                (IRIf {}, _)-> return ()
                (IRGoto {}, _) -> return ()
                (IRRet {}, _) -> return () 
                _ -> do
                    removeNextInstr
                    genInstrs lbl i
                    expireOld lm                
                    doSteps

secondPassOnBlock :: Label -> CGMonad ()
secondPassOnBlock lbl = do
    instr <- getNextInstr
    case instr of 
        Nothing -> return ()
        Just i@(_, lm) -> do
            removeNextInstr
            genInstrs lbl i
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
        saveCurrentNru b
        firstPass bs v1

secondPass :: [Label] -> Set Label -> CGMonad ()
secondPass [] _ = return ()
secondPass (b:bs) visited
    | b `S.member` visited = secondPass bs visited
    | otherwise = do
        restoreNru b
        v1 <- passOnFunc secondPassOnBlock (S.singleton b) visited
        secondPass bs v1


collectCode :: Label -> CGMonad [AsmInstr]
collectCode lbl = do
    CGMS {generatedCode = gc} <- getMs lbl
    return $ reverse gc

collectAllCode :: [Label] -> CGMonad [AsmInstr]
collectAllCode = foldM (\acc lbl -> do
    code <- collectCode lbl
    return $ acc ++ code) []


generateStrings :: CGMonad [AsmInstr]
generateStrings = do
    s2l <- getStringMapping
    return $ concatMap (\s -> [Lbl (s2l ! s), Str s]) (M.keys s2l)

generatePrologue :: CGMonad [AsmInstr]
generatePrologue = do
    strings <- generateStrings
    return $ (Section ".data"):strings ++ 
             [ Section ".text"
             , P2Align 
             , Global "main"
             , Function "main" ]


genCode :: [Label] -> CGMonad [AsmInstr]
genCode ord = do
    firstPass ord S.empty
    secondPass ord S.empty
    prologue <- generatePrologue
    functionsCode <- collectAllCode ord
    let code = prologue ++ functionsCode
    return $ optimizePeepHole code

generateAsm :: LiveState -> [Label] -> [AsmInstr]
generateAsm ls ord = evalState (genCode ord) (initialCGState ls ord)
