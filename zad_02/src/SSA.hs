module SSA where

import IRDef
import CFG ( CFGBlock(..), CFGState(..), Phi )

import Prelude hiding ( lookup )

import Control.Monad.State
import Control.Monad.Reader
import Data.List ( (\\) )
import Data.Map hiding ( map, (\\) )


data SSAState = SSAS {
    blockOrder :: [Label],
    blocks :: Map Label CFGBlock,
    varMapping :: Map (Label, IRAddr) IRAddr,
    visitedBlocks :: [Label],
    nextVal :: Integer
}

type SSAMonad = StateT SSAState (Reader Phi)

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


findMappings :: IRAddr -> [Label] -> [Label] -> SSAMonad [(Label, IRAddr)]
findMappings _ _ [] = return []
findMappings var vl (l:ls) = do
    visited <- isVisited l
    unless visited $ transformBlock l
    value <- getMapping (l, var)
    B _ _ _ pb <- getBlock l
    case value of
        Just val -> do
            nextMappings <- findMappings var evl ls
            return $ (l, val):nextMappings
        Nothing -> findMappings var evl (ls ++ (pb \\ vl))
    where evl = (l:vl)


findValue :: IRAddr -> Label -> Phi -> SSAMonad (IRAddr, Phi)
findValue var lbl phi = 
    case var of 
        Indirect _ -> do
            values <- findMappings var [] [lbl]
            case values of 
                [(_, val)] -> do
                    setMapping (lbl, var) val
                    return (val, phi)
                _ -> do
                    nv <- freshVal (addrType var)
                    setMapping (lbl, var) nv
                    return $ (nv, insert nv values phi)
        _ -> return (var, phi)


trBl :: Label -> [IRInstr] -> [IRInstr] -> SSAMonad ()
trBl lbl [] acc = do
    let instrs = reverse acc
    phi <- ask
    B _ _ nb pb <- getBlock lbl
    setBlock lbl $ B phi instrs nb pb
trBl lbl (i:is) acc = do
    phi <- ask
    case i of
        IRAss op dst larg rarg -> do
            (lval, p1) <- findValue larg lbl phi
            (rval, p2) <- findValue rarg lbl p1
            newval <- freshVal (addrType dst)
            setMapping (lbl, dst) newval
            let newinstr = IRAss op newval lval rval
            local (const p2) $ trBl lbl is (newinstr:acc)
        IRSAss op dst arg -> do
            (argval, p1) <- findValue arg lbl phi
            newval <- freshVal (addrType dst)
            setMapping (lbl, dst) newval
            let newinstr = IRSAss op newval argval
            local (const p1) $ trBl lbl is (newinstr:acc)
        -- reminder: NoRet should not be added to mapping
        IRCall dst fun args -> do
            (argvals, p2) <- foldM (\(a, p) ar -> do 
                (a1, p1) <- findValue ar lbl p
                return ((a1:a), p1) ) ([], phi) args
            newval <- freshVal (addrType dst)
            let newinstr = IRCall newval fun (reverse argvals)
            unless (newval == NoRet) $ setMapping (lbl, dst) newval
            local (const p2) $ trBl lbl is (newinstr:acc) 
        IRIf cmp larg rarg lTrue lFalse -> do
            (lval, p1) <- findValue larg lbl phi
            (rval, p2) <- findValue rarg lbl p1
            let newinstr = IRIf cmp lval rval lTrue lFalse
            local (const p2) $ trBl lbl is (newinstr:acc)
        IRGoto l -> trBl lbl is ((IRGoto l):acc)
        IRCpy dst arg -> do
            (argval, p1) <- findValue arg lbl phi
            setMapping (lbl, dst) argval
            local (const p1) $ trBl lbl is acc
        IRLabel l -> trBl lbl is ((IRLabel l):acc) 
        IRRet arg -> do
            (argval, p1) <- findValue arg lbl phi
            let newinstr = IRRet argval
            local (const p1) $ trBl lbl is (newinstr:acc)       
        IRParam var -> do
            setMapping (lbl, Indirect var) (Indirect var)
            trBl lbl is ((IRParam var):acc)




transformBlock :: Label -> SSAMonad ()
transformBlock lbl = do
    visited <- isVisited lbl
    unless visited $ do
        addVisited lbl
        B _ is nb _ <- getBlock lbl
        trBl lbl is []
        transformBlocks nb


transformBlocks :: [Label] -> SSAMonad ()
transformBlocks = mapM_ transformBlock

convertToSSA :: CFGState -> CFGState
convertToSSA st@(CFGS bo _) = CFGS nbo nbl where 
    (SSAS nbo nbl _ _ _) = runReader (execStateT (transformBlocks bo) (initialSSAState st)) empty
