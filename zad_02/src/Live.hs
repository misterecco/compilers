module Live where

import IRDef
import CFG ( CFGBlock(..), CFGState(..), Phi )

import Control.Monad.State
import Data.Map as M hiding ( foldr )
import Data.Set as S hiding ( foldr )
import Data.List ( intercalate )

type LiveMap = Map IRAddr Integer
type BlockProcess = (IRAddr -> [(Label, IRAddr)] -> Set IRAddr -> Set IRAddr)

data LiveState = LS {
    blocks :: Map Label CFGBlock,
    liveIn :: Map Label (Set IRAddr),
    extBlocks :: Map Label LiveBlock
}

data LiveBlock = LB {
    blockPhi :: Phi,
    instructions :: [(IRInstr, LiveMap)],
    nextBlocks :: [Label],
    prevBlocks :: [Label]
}

instance Show LiveBlock where
    show (LB phi instrs nb pb) = let
        showInstr :: (IRInstr, LiveMap) -> String
        showInstr (i, m) = show i ++ "\n" ++ show m ++ "\n"
        in
        unlines 
            [ "Before: " ++ intercalate ", " pb
            , "After: " ++ intercalate ", " nb
            , "Phi: " ++ show (M.toList phi)
            , "Instructions: "
            , unlines (Prelude.map showInstr instrs) ]

type LiveMonad = State LiveState

initialLiveState :: CFGState -> LiveState
initialLiveState (CFGS bo bl) = do
    let emptySetList = S.empty:emptySetList
    let emptyLives = zip bo emptySetList
    LS bl (M.fromList emptyLives) M.empty

getBlock :: Label -> LiveMonad CFGBlock
getBlock lbl = do
    LS bls _ _ <- get
    return $ bls ! lbl

setBlock :: Label -> CFGBlock -> LiveMonad ()
setBlock lbl block = do
    LS bls lm ebl <- get
    put $ LS (M.insert lbl block bls) lm ebl

getLiveMap :: LiveMonad (Map Label (Set IRAddr))
getLiveMap = do
    LS _ lm _ <- get
    return lm

getLiveIn :: Label -> LiveMonad (Set IRAddr)
getLiveIn lbl = do
    lm <- getLiveMap
    return $ lm ! lbl

setLiveIn :: Label -> Set IRAddr -> LiveMonad ()
setLiveIn lbl lives = do
    LS bl lm ebl <- get
    put $ LS bl (M.insert lbl lives lm) ebl

setExtendedBlock :: Label -> LiveBlock -> LiveMonad ()
setExtendedBlock lbl blck = do
    LS bls lm ebls <- get
    put $ LS bls lm (M.insert lbl blck ebls)

getLiveOut :: Label -> LiveMonad (Set IRAddr)
getLiveOut lbl = do
    B _ _ nb _ <- getBlock lbl
    nextIns <- mapM getLiveIn nb
    let outs = foldr S.union S.empty nextIns
    outs1 <- foldM (processBlock minusDef) outs nb
    foldM (processBlock plusArgs) outs1 nb
    where
        processBlock :: BlockProcess -> Set IRAddr -> Label -> LiveMonad (Set IRAddr)
        processBlock blockFunc acc l = do
            B phi _ _ _ <- getBlock l
            return $ foldrWithKey blockFunc acc phi
        minusDef :: BlockProcess
        minusDef k _v = S.delete k
        plusArgs :: BlockProcess
        plusArgs _k v a = foldr processAssignment a v
        processAssignment :: (Label, IRAddr) -> Set IRAddr -> Set IRAddr
        processAssignment (src, addr) acc = case addr of
            Indirect _ -> if lbl == src then S.insert addr acc else acc
            _ -> acc

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
    IRParam arg _ -> addAddr arg outs

calcInstrs :: [IRInstr] -> Set IRAddr -> LiveMonad (Set IRAddr)
calcInstrs [] ins = return ins
calcInstrs (i:is) outs = do
    ins <- calcInstr i outs 
    calcInstrs is ins

calcBlock :: Label -> LiveMonad ()
calcBlock lbl = do
    B _ instrs _ _ <- getBlock lbl
    outs <- getLiveOut lbl
    ins <- calcInstrs (reverse instrs) outs
    setLiveIn lbl ins

calculateBlocksLiveIns :: [Label] -> LiveMonad ()
calculateBlocksLiveIns bls = do
    preLm <- getLiveMap
    mapM_ calcBlock bls
    postLm <- getLiveMap
    unless (postLm == preLm) $ calculateBlocksLiveIns bls


extAddAddr :: IRAddr -> LiveMap -> LiveMonad LiveMap
extAddAddr addr lm = case addr of
    (Indirect _) -> return $ M.insert addr 0 lm
    _ -> return lm

extRemAddr :: IRAddr -> LiveMap -> LiveMonad LiveMap
extRemAddr addr lm = case addr of
    (Indirect _) -> return $ M.delete addr lm
    _ -> return lm

increaseKeys :: LiveMap -> LiveMap
increaseKeys = mapWithKey (\_ v -> (v+1))

extCalcInstr :: IRInstr -> LiveMap -> LiveMonad LiveMap
extCalcInstr instr lives = case instr of
    IRAss _ dst larg rarg -> do
        l1 <- extAddAddr larg lives
        l2 <- extAddAddr rarg l1
        extRemAddr dst l2
    IRSAss _ dst arg -> do
        l1 <- extAddAddr arg lives
        extRemAddr dst l1
    IRCall dst _ args -> do
        l1 <- foldM (\o ar -> extAddAddr ar o) lives args
        extRemAddr dst l1
    IRIf _ larg rarg _ _ -> do
        l1 <- extAddAddr larg lives
        extAddAddr rarg l1
    IRGoto _ -> return lives
    IRCpy dst arg -> do
        l1 <- extAddAddr arg lives
        extRemAddr dst l1
    IRLabel _ -> return lives
    IRRet arg -> extAddAddr arg lives
    IRParam arg _ -> extAddAddr arg lives

extCalcInstrs :: [IRInstr] -> LiveMap -> [(IRInstr, LiveMap)] -> LiveMonad [(IRInstr, LiveMap)]
extCalcInstrs [] _lives acc = return acc
extCalcInstrs (i:is) lives acc = do
    let updatedLives = increaseKeys lives
    newLives <- extCalcInstr i updatedLives
    extCalcInstrs is newLives ((i, lives):acc)

extCalcBlock :: Label -> LiveMonad ()
extCalcBlock lbl = do
    B p instrs nb pb <- getBlock lbl
    outs <- getLiveOut lbl
    let zeros = 0:zeros
    let initialLives = M.fromList $ zip (S.toList outs) zeros
    extInstrs <- extCalcInstrs (reverse instrs) initialLives []
    setExtendedBlock lbl $ LB p extInstrs nb pb

calculateInstrLives :: [Label] -> LiveMonad ()
calculateInstrLives = mapM_ extCalcBlock


remUnreachInstrs :: [IRInstr] -> [IRInstr] -> LiveMonad [IRInstr]
remUnreachInstrs [] acc = return $ reverse acc
remUnreachInstrs (i:is) acc = case i of
    IRRet _ -> remUnreachInstrs [] (i:acc)
    _ -> remUnreachInstrs is (i:acc)

remUnreachInstrsInBlock :: Label -> LiveMonad ()
remUnreachInstrsInBlock lbl = do
    B p instrs nb pb <- getBlock lbl
    newInstrs <- remUnreachInstrs instrs []
    setBlock lbl $ B p newInstrs nb pb
    return ()

remUnreachInstrsInBlocks :: [Label] -> LiveMonad ()
remUnreachInstrsInBlocks = mapM_ remUnreachInstrsInBlock


calculateLiveIns :: [Label] -> LiveMonad ()
calculateLiveIns bls = do
    remUnreachInstrsInBlocks bls
    calculateBlocksLiveIns bls
    calculateInstrLives bls

calculateLiveliness :: CFGState -> LiveState
calculateLiveliness st@(CFGS bo _) = execState (calculateLiveIns (reverse bo)) (initialLiveState st)
